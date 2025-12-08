if [[ $- == *i* ]]; then
	function __insert_pwd_at_point() {
		READLINE_LINE="${READLINE_LINE::$READLINE_POINT}$PWD${READLINE_LINE:$READLINE_POINT}"
		((READLINE_POINT += ${#PWD}))
	}
	bind -x '"\em":__insert_pwd_at_point'
fi

function commands_exist() {
	if [[ $# -eq 0 ]]; then
		echo "${FUNCNAME[0]}: no commands given, usage: ${FUNCNAME[0]} COMMAND [COMMANDS]..."
		false
		return
	fi
	for cmd in "$@"; do
		if ! command -v "$cmd" >/dev/null; then
			echo "${FUNCNAME[0]}: $cmd not found!"
			false
			return
		fi
	done
}

# some more ls aliases
alias ls='ls --color=never'
alias ll='ls --color=never -halF'
alias la='ls --color=never -hAF'
alias l='ls --color=never -hCF'

alias grep='grep --color=never'

if commands_exist fvm; then
	# use fvm when using flutter or dart
	alias dart='fvm dart'
	alias flutter='fvm flutter'
fi

if commands_exist git; then
	alias g='git status'
	alias gd='git diff'
	alias gdh='git diff HEAD'
	alias merge-conflict-files='git ls-files --unmerged | awk '\''{print $NF}'\'' | uniq'
fi

# nice alias
alias ..='cd ..'

if commands_exist kak; then
	export EDITOR=$(which kak)

	function k() {
		kak -clear # clear dead sessions
		local kak_session_name kak_flag
		if commands_exist tmux && [[ -n "$TMUX" ]]; then
			# if in tmux session, kak session takes the name of tmux session
			kak_session_name=$(tmux display-message -p '#S')
		else
			kak_session_name='non_tmux'
		fi
		if [[ -n $(kak -l | grep "$kak_session_name" | head -n1) ]]; then
			kak_flag='-c' # attach to session
		else
			kak_flag='-s' # start new session
		fi
		kak "$kak_flag" "$kak_session_name" "$@"
	}
fi

# make escape key timeout super short
set keyset-timeout 1

function __set_prompt() {
	local info_prefix="## "
	local command_prefix=":; "
	local info_line="$info_prefix[\t] [\j] [\$?]"
	local pwd_line="\n$info_prefix\w"
	local git_line="\$(__git_ps1 '\n$info_prefix%s')"
	PS1="\n$info_line$pwd_line$git_line\n$command_prefix"
	PS2="$command_prefix"
	PS3="$command_prefix"
}
__set_prompt

export NO_COLOR=1
unset LS_COLORS

function __cat_greeting() {
	local cats_home="$HOME/cats"
	if [[ -d $cats_home ]]; then
		local num_cats
		num_cats=$(find "$cats_home" -maxdepth 1 -type f -name 'cat*' | wc -l)
		local the_cat="$HOME/cats/cat$((RANDOM % num_cats))"
		[[ -f $the_cat ]] && cat "$the_cat"
	fi
}
__cat_greeting

# shellcheck source=/dev/null
commands_exist fzf && source <(fzf --bash)

if commands_exist xsel; then
	function copy-to-system-clipboard() {
		xsel -ib
	}
fi

if commands_exist tmux; then
	# create new tmux session with name of specified directory and start dir there or if no directory arg is given, start at current directory
	function t() {
		local dir session
		if [[ -n "$1" ]]; then
			dir="$(realpath "$1")"
			shift
		else
			dir="$PWD"
		fi
		session="$(basename "$dir")"
		session="${session//./_}" # tmux session names cannot have .
		session="${session//+/_}" # kakoune session names cannot have +
		if ! tmux has-session -t "$session" 2>/dev/null; then
			tmux new-session -d -c "$dir" -s "$session"
		fi
		if [[ -n "$TMUX" ]]; then
			tmux switch-client -t "$session" "$@"
		else
			tmux attach-session -t "$session" "$@"
		fi
	}

	if commands_exist copy-to-system-clipboard; then
		function copy-tmux-buffer-to-system-clipboard() {
			tmux showb | copy-to-system-clipboard
		}
	fi

	_t_completions() {
		# only complete first arg after command
		if [ "${#COMP_WORDS[@]}" -ne 2 ]; then
			return
		fi

		# suggest tmux session names
		local tmux_sessions
		mapfile -t tmux_sessions < <(IFS=$'\n' compgen -W "$(tmux ls)" -- "${COMP_WORDS[COMP_CWORD]}")
		if [ "${#tmux_sessions[@]}" -eq 1 ]; then
			COMPREPLY=("${tmux_sessions[0]%%: *}")
		else
			COMPREPLY=("${tmux_sessions[@]}")
		fi
	}
	# suggest tmux session names and directories
	complete -F _t_completions -A directory t
fi

if commands_exist clj; then
	alias cljd='clojure -M:cljd'
fi

function find-grep() {
	# find -type f -iname "$1" -exec grep -Hn "$2" {} \;
	find -type f -iname "$1" -exec grep --with-filename --line-number "$2" {} \;
}

# case-insensitive search in less
LESS=-i

# for history
HISTCONTROL='ignoreboth:erasedups'
PROMPT_COMMAND='history -a; history -c; history -r;'
