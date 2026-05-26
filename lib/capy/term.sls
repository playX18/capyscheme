(library (capy term)
  (export make-command command? command-name command->string
          queue-command queue-commands execute-command execute-commands
          begin-synchronized-update end-synchronized-update with-synchronized-update
          move-to move-to-next-line move-to-previous-line move-to-column move-to-row
          move-up move-right move-down move-left
          save-position restore-position hide-cursor show-cursor
          enable-blinking disable-blinking set-cursor-style cursor-move-to cursor-position
          rgb rgb? rgb-r rgb-g rgb-b
          ansi-color ansi-color? ansi-color-n
          make-content-style content-style? content-style-foreground
          content-style-background content-style-underline-color
          content-style-attributes
          styled-content? styled-content-text styled-content-style
          style with-style stylize
          set-foreground-color set-background-color set-underline-color
          set-colors reset-color set-attribute set-attributes set-style
          print print-styled-content start-hyperlink end-hyperlink
          available-color-count force-color-output! color-output-forced?
          disable-line-wrap enable-line-wrap
          enter-alternate-screen leave-alternate-screen
          scroll-up scroll-down clear terminal-clear set-size set-title
          enable-raw-mode! disable-raw-mode! raw-mode-enabled?
          terminal-size terminal-window-size window-size? window-size-columns
          window-size-rows window-size-width window-size-height
          focus-gained-event focus-lost-event key-event mouse-event paste-event resize-event
          focus-gained-event? focus-lost-event? key-event? mouse-event? paste-event? resize-event?
          key-event-code key-event-modifiers key-event-kind key-event-state
          mouse-event-kind mouse-event-column mouse-event-row mouse-event-modifiers
          resize-event-columns resize-event-rows
          key-press-event? key-release-event? key-repeat-event?
          event-key event-mouse event-paste event-resize
          function-key? key-char media-key? modifier-key?
          enable-mouse-capture disable-mouse-capture
          enable-focus-change disable-focus-change
          enable-bracketed-paste disable-bracketed-paste
          push-keyboard-enhancement-flags pop-keyboard-enhancement-flags
          poll-event read-event try-read-event
          parse-event
          clipboard-selection copy-to-clipboard
          tty? ansi-supported? open-tty-input-fd open-tty-output-fd open-tty-fd
          make-line-state line-state? line-state-prompt line-state-buffer
          line-state-cursor line-state-history line-state-history-index
          line-state-completer line-state-renderer line-state-result
          make-line-editor line-editor? line-editor-history line-editor-completer
          line-editor-renderer
          line-completion-menu
          line-editor-add-history! line-edit-step read-line/edit)
  (import (except (capy term command) begin-synchronized-update end-synchronized-update)
          (capy term cursor)
          (capy term style)
          (capy term terminal)
          (capy term event)
          (capy term event-parser)
          (capy term clipboard)
          (capy term tty)
          (capy term line)))
