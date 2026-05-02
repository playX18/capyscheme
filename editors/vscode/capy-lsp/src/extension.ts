import * as os from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let extensionContext: vscode.ExtensionContext | undefined;
let outputChannel: vscode.OutputChannel | undefined;
let clientDisposables: vscode.Disposable[] = [];
let actionOutputProvider: ActionOutputProvider | undefined;

interface CapyActionArgs {
  uri: string;
  action: string;
  range?: unknown;
  title?: string;
}

interface CapyActionOutput {
  title: string;
  language: string;
  content: string;
}

const capyActions = [
  { action: 'capy.lsp.action.expand', title: 'Expand' }
];

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  extensionContext = context;
  outputChannel = vscode.window.createOutputChannel('Capy LSP');
  actionOutputProvider = new ActionOutputProvider();
  context.subscriptions.push(outputChannel);
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider('capy-lsp-output', actionOutputProvider)
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('capyLsp.start', startClientFromCommand),
    vscode.commands.registerCommand('capyLsp.stop', stopClient),
    vscode.commands.registerCommand('capyLsp.restart', restartClient),
    vscode.commands.registerCommand('capyLsp.enable', enableClient),
    vscode.commands.registerCommand('capyLsp.disable', disableClient),
    vscode.commands.registerCommand('capyLsp.showOutput', () => outputChannel?.show(true)),
    vscode.commands.registerCommand('capyLsp.runAction', runAction),
    vscode.workspace.onDidChangeConfiguration(async event => {
      if (event.affectsConfiguration('capyLsp')) {
        if (isEnabled()) {
          await restartClient();
        } else {
          await stopClient();
        }
      }
    })
  );

  await startClient(context);
}

export async function deactivate(): Promise<void> {
  await stopClient();
}


async function restartClient(): Promise<void> {
  const context = extensionContext;
  await stopClient();
  if (context && isEnabled()) {
    await startClient(context);
  }
}

async function startClientFromCommand(): Promise<void> {
  const context = extensionContext;
  if (!context) {
    return;
  }
  if (!isEnabled()) {
    await setEnabled(true);
  }
  await startClient(context);
}

async function enableClient(): Promise<void> {
  await setEnabled(true);
  await startClientFromCommand();
}

async function disableClient(): Promise<void> {
  await setEnabled(false);
  await stopClient();
}

async function startClient(context: vscode.ExtensionContext): Promise<void> {
  if (client) {
    return;
  }
  if (!isEnabled()) {
    outputChannel?.appendLine('Capy LSP is disabled by capyLsp.enabled.');
    return;
  }

  const config = vscode.workspace.getConfiguration('capyLsp');
  const serverPath = resolveConfiguredValue(config.get('serverPath', 'capy-lsp'), context);
  const serverArgs = config.get<string[]>('serverArgs', []);
  const vmPath = resolveConfiguredValue(config.get('vmPath', 'capy-lsp-vm'), context);
  const extraEnv = readExtraEnv(config.get<Record<string, unknown>>('extraEnv', {}), context);
  const env: NodeJS.ProcessEnv = {
    ...process.env,
    ...extraEnv
  };

  if (vmPath.trim().length > 0) {
    env.CAPY_LSP_VM = vmPath;
  }

  const fileWatchers = [
    vscode.workspace.createFileSystemWatcher('**/lsp-config.scm'),
    vscode.workspace.createFileSystemWatcher('**/*.{scm,sls,sld,sps}')
  ];
  clientDisposables = fileWatchers;

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: serverArgs,
    options: {
      env
    }
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: 'file', language: 'capy-scheme' },
      { scheme: 'file', language: 'scheme' }
    ],
    synchronize: {
      fileEvents: fileWatchers
    },
    outputChannel
  };

  const nextClient = new LanguageClient(
    'capyLsp',
    'Capy Scheme LSP',
    serverOptions,
    clientOptions
  );

  client = nextClient;
  try {
    await nextClient.start();
  } catch (error) {
    client = undefined;
    disposeClientResources();
    const message = error instanceof Error ? error.message : String(error);
    outputChannel?.appendLine(`Failed to start Capy LSP: ${message}`);
    await vscode.window.showErrorMessage(`Capy LSP failed to start: ${message}`);
  }
}

async function stopClient(): Promise<void> {
  const oldClient = client;
  client = undefined;
  disposeClientResources();
  if (oldClient) {
    await oldClient.stop();
  }
}

async function runAction(args?: CapyActionArgs): Promise<void> {
  const currentClient = client;
  if (!currentClient) {
    await vscode.window.showErrorMessage('Capy LSP is not running.');
    return;
  }
  const actionArgs = isActionArgs(args) ? args : await promptActionArgs();
  if (!actionArgs) {
    return;
  }

  try {
    const output = await currentClient.sendRequest<CapyActionOutput>('workspace/executeCommand', {
      command: actionArgs.action,
      arguments: [actionArgs]
    });
    await showActionOutput(actionArgs, output);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    outputChannel?.appendLine(`Capy LSP action failed: ${message}`);
    await vscode.window.showErrorMessage(`Capy LSP action failed: ${message}`);
  }
}

function isActionArgs(args: CapyActionArgs | undefined): args is CapyActionArgs {
  return !!args && typeof args.action === 'string' && typeof args.uri === 'string';
}

async function promptActionArgs(): Promise<CapyActionArgs | undefined> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    await vscode.window.showErrorMessage('Open a Scheme document before running a Capy LSP action.');
    return undefined;
  }

  const picked = capyActions[0];

  const args: CapyActionArgs = {
    uri: editor.document.uri.toString(),
    action: picked.action,
    title: picked.title
  };
  if (!editor.selection.isEmpty) {
    args.range = toLspRange(editor.selection);
  }
  return args;
}

function toLspRange(range: vscode.Range): unknown {
  return {
    start: {
      line: range.start.line,
      character: range.start.character
    },
    end: {
      line: range.end.line,
      character: range.end.character
    }
  };
}

async function showActionOutput(args: CapyActionArgs, output: CapyActionOutput): Promise<void> {
  const provider = actionOutputProvider;
  if (!provider) {
    return;
  }

  const key = provider.set(output.content);
  const source = vscode.Uri.parse(args.uri);
  const sourceName = path.basename(source.fsPath || source.path || 'capy');
  const title = sanitizePathSegment(output.title || args.title || 'Capy LSP Output');
  const uri = vscode.Uri.from({
    scheme: 'capy-lsp-output',
    path: `/${sanitizePathSegment(sourceName)}-${title}.scm`,
    query: key
  });
  const document = await vscode.workspace.openTextDocument(uri);
  const language = output.language || 'scheme';
  const typedDocument =
    document.languageId === language
      ? document
      : await vscode.languages.setTextDocumentLanguage(document, language);
  await vscode.window.showTextDocument(typedDocument, {
    preview: false,
    viewColumn: vscode.ViewColumn.Beside
  });
}

function sanitizePathSegment(value: string): string {
  return value.replace(/[^A-Za-z0-9._-]+/g, '-').replace(/^-+|-+$/g, '') || 'output';
}

function disposeClientResources(): void {
  for (const disposable of clientDisposables) {
    disposable.dispose();
  }
  clientDisposables = [];
}

class ActionOutputProvider implements vscode.TextDocumentContentProvider {
  private readonly outputs = new Map<string, string>();
  private nextId = 1;
  private readonly changeEmitter = new vscode.EventEmitter<vscode.Uri>();
  readonly onDidChange = this.changeEmitter.event;

  provideTextDocumentContent(uri: vscode.Uri): string {
    return this.outputs.get(uri.query) ?? '';
  }

  set(content: string): string {
    const key = String(this.nextId++);
    this.outputs.set(key, content);
    return key;
  }
}

function readExtraEnv(
  raw: Record<string, unknown>,
  context: vscode.ExtensionContext
): Record<string, string> {
  const env: Record<string, string> = {};
  for (const [key, value] of Object.entries(raw ?? {})) {
    if (typeof value === 'string') {
      env[key] = resolveConfiguredValue(value, context);
    }
  }
  return env;
}

function isEnabled(): boolean {
  return vscode.workspace.getConfiguration('capyLsp').get('enabled', true);
}

async function setEnabled(enabled: boolean): Promise<void> {
  await vscode.workspace
    .getConfiguration('capyLsp')
    .update('enabled', enabled, vscode.ConfigurationTarget.Workspace);
}

function resolveConfiguredValue(value: string, context: vscode.ExtensionContext): string {
  let resolved = value.trim();
  const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';
  resolved = resolved
    .replace(/\$\{workspaceFolder\}/g, workspaceFolder)
    .replace(/\$\{extensionPath\}/g, context.extensionPath);

  if (resolved === '~') {
    return os.homedir();
  }
  if (resolved.startsWith(`~${path.sep}`) || resolved.startsWith('~/')) {
    return path.join(os.homedir(), resolved.slice(2));
  }
  return resolved;
}
