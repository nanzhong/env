function git_is_repo -d "Check if directory is a repository"
  test -d .git; or command git rev-parse --git-dir >/dev/null ^/dev/null
end

function git_branch_name -d "Get current branch name"
  command git symbolic-ref --short HEAD
end

function git_is_touched -d "Check if repo has any changes"
  test -n (echo (command git status --porcelain))
end

function fish_right_prompt
  set -l code $status

  function prompt::status::colour -S
    test $code -ne 0; and prompt::colour::err; or prompt::colour::snd
  end
  if git_is_repo
    echo (prompt::status::colour)" ≡ "(prompt::colour::dim)(git_branch_name)(prompt::colour::off)
  else
    echo (prompt::status::colour)" ≡"(prompt::colour::off)
  end
end
