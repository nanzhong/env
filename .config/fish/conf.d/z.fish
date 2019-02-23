set -x ZLUA_SCRIPT "/root/src/z.lua/z.lua"
set -x ZLUA_LUAEXE "/usr/bin/lua"
function _zlua
	set -l arg_mode ""
	set -l arg_type ""
	set -l arg_subdir ""
	set -l arg_inter ""
	set -l arg_strip ""
	function _zlua_call; eval (string escape -- $argv); end
	if test "$argv[1]" = "--add"
		set -e argv[1]
		set -x _ZL_RANDOM (random)
		_zlua_call "$ZLUA_LUAEXE" "$ZLUA_SCRIPT" --add $argv
		return
	else if test "$argv[1]" = "--complete"
		set -e argv[1]
		_zlua_call "$ZLUA_LUAEXE" "$ZLUA_SCRIPT" --complete $argv
		return
	end
	while true
		switch "$argv[1]"
			case "-l"; set arg_mode "-l"
			case "-e"; set arg_mode "-e"
			case "-x"; set arg_mode "-x"
			case "-t"; set arg_type "-t"
			case "-r"; set arg_type "-r"
			case "-c"; set arg_subdir "-c"
			case "-s"; set arg_strip "-s"
			case "-i"; set arg_inter "-i"
			case "-I"; set arg_inter "-I"
			case "-h"; set arg_mode "-h"
			case "--help"; set arg_mode "-h"
			case "--purge"; set arg_mode "--purge"
			case '*'; break
		end
		set -e argv[1]
	end
	if test "$arg_mode" = "-h"
		_zlua_call "$ZLUA_LUAEXE" "$ZLUA_SCRIPT" -h
	else if test "$arg_mode" = "--purge"
		_zlua_call "$ZLUA_LUAEXE" "$ZLUA_SCRIPT" --purge
	else if test "$arg_mode" = "-l"
		_zlua_call "$ZLUA_LUAEXE" "$ZLUA_SCRIPT" -l $arg_subdir $arg_type $arg_strip $argv
	else if test (count $argv) -eq 0
		_zlua_call "$ZLUA_LUAEXE" "$ZLUA_SCRIPT" -l $arg_subdir $arg_type $arg_strip $argv
	else if test -n "$arg_mode"
		_zlua_call "$ZLUA_LUAEXE" "$ZLUA_SCRIPT" $arg_mode $arg_subdir $arg_type $arg_inter $argv
	else
		set -l dest (_zlua_call "$ZLUA_LUAEXE" "$ZLUA_SCRIPT" --cd $arg_type $arg_subdir $arg_inter $argv)
		if test -n "$dest" -a -d "$dest"
			if test -z "$_ZL_CD"
				builtin cd "$dest"
			else
				_zlua_call "$_ZL_CD" "$dest"
			end
			if test -n "$_ZL_ECHO"; pwd; end
		end
	end
end

if test -z "$_ZL_CMD"; set -x _ZL_CMD z; end
alias "$_ZL_CMD"=_zlua

function _zlua_precmd --on-event fish_prompt
	_zlua --add "$PWD" 2> /dev/null &
end

function _z_complete
	eval "$_ZL_CMD" --complete (commandline -t)
end

complete -c $_ZL_CMD -f -a '(_z_complete)'
complete -c $_ZL_CMD -s 'r' -d 'cd to highest ranked dir matching'
complete -c $_ZL_CMD -s 'i' -d 'cd with interactive selection'
complete -c $_ZL_CMD -s 'I' -d 'cd with interactive selection using fzf'
complete -c $_ZL_CMD -s 't' -d 'cd to most recently accessed dir matching'
complete -c $_ZL_CMD -s 'l' -d 'list matches instead of cd'
complete -c $_ZL_CMD -s 'c' -d 'restrict matches to subdirs of $PWD'
complete -c $_ZL_CMD -s 'e' -d 'echo the best match, don''t cd'
complete -c $_ZL_CMD -s 'b' -d 'jump backwards to given dir or to project root'
complete -c $_ZL_CMD -s 'x' -x -d 'remove path from history' -a '(_z_complete)'

