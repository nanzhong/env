function fish_greeting
    if test -z "$init_org"
        if test ! -d "$HOME/org"
            echo "[!] Org repo missing, some emacs configurations will not work."
            echo "    1. Clone org repo:"
            echo "       git clone git@github.com:nanzhong/org.git"
            echo
            echo "    - To skip this reminder:"
            echo "      set -U init_org true"
        else
            set -U init_org true
        end
    end

    if test -z "$init_do_calendar_diary"
        if test ! -e "$HOME/.config/emacs/do.diary"
            echo "[!] Work calendar export to emacs diary not configured."
            echo "    1. Install gcal2diary:"
            echo "       go install github.com/nanzhong/gcal2diary/cmd/gcal2diary@latest"
            echo "    2. Configure client credentials:"
            echo "       op get document 'gcal2diary Client Credentials' --output ~/.config/gcal2diary/credentials.json"
            echo "    3. Setup cron on schedule"
            echo "       */5 * * * * gcal2diary > ~/.config/emacs/do.diary"
            echo
            echo "    - To skip this reminder:"
            echo "      set -U init_do_calendar_diary true"
        else
            set -U init_do_calendar_diary true
        end
    end
end
