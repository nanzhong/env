function prompt::colour::fst; set_color -o 6af; end
function prompt::colour::snd; set_color -o cf3; end
function prompt::colour::trd; set_color -o aaa; end
function prompt::colour::dim; set_color -o 888; end
function prompt::colour::err; set_color -o f30; end
function prompt::colour::off; set_color normal; end

function git_is_repo -d "Check if directory is a repository"
    test -d .git; or command git rev-parse --git-dir >/dev/null ^/dev/null
end

function git_branch_name -d "Get current branch name"
    command git symbolic-ref --short HEAD
end

function git_is_touched -d "Check if repo has any changes"
    test -n (echo (command git status --porcelain))
end

function fish_prompt
    set -l code $status
    set -l base (basename "$PWD")

    function prompt::status::colour -S
        test $code -ne 0; and prompt::colour::err; or prompt::colour::snd
    end

    if test "$PWD" != "/"
        prompt_pwd | sed "s|$base|"(prompt::colour::trd)"$base"(prompt::colour::off)"|g" \
        | sed "s|~|"(prompt::colour::fst)"~"(prompt::colour::off)"|g" \
        | sed "s|/|"(prompt::colour::snd)"/"(prompt::colour::off)(prompt::colour::dim)"|g" \
        | tr -d \n
    else
        printf (prompt::colour::fst)"/"(prompt::colour::off)
    end
    if git_is_repo
        printf " ≡ "(prompt::colour::dim)(git_branch_name)(prompt::colour::off)
    end
    echo (prompt::colour::off)
    echo (prompt::status::colour)"› "(prompt::colour::off)
end
