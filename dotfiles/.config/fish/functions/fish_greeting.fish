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
end
