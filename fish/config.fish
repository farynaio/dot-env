set -q XDG_DATA_HOME
and set -gx OMF_PATH "$XDG_DATA_HOME.omg"
or set -gx OMF_PATH "$HOME/.local/share/omf"

#source $OMF_PATH/init.fish
#eval "bash ~/.bash_profile"

function mySetEnv
  if [ $argv[1] = PATH ]
    set -gx PATH (echo $argv[2] | tr ': ' \n)
else
  set -gx $argv;
end
end

source ~/.env

function subl
  mvim +NERDTree
end

#alias subl="~/bin/subl"
#alias subl="/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl "
alias vi="/usr/local/bin/vim"
alias stree="/Applications/SourceTree.app/Contents/Resources/stree"
alias vie="/usr/local/bin/vim -R"
alias tree="/usr/local/bin/tree -C "
#alias j="z"

set -U fish_user_paths ~/bin $fish_user_paths
set -U fish_user_paths ~/.npm-packages/bin $fish_user_paths

#. ~/.config/fish/z.fish

set -Ux EDITOR /usr/local/bin/vim


#eval (docker-machine env docker-vm)

function fish_prompt
  set -l textcol green
  set -l bgcol black
  set -l textVcs yellow
  set_color $textcol -b $bgcol
  set -l git_branch (git branch ^/dev/null | sed -n '/\* /s///p')

  if test $git_branch
    echo -n (hostname)':'(prompt_pwd) '('
    set_color $textVcs -b $bgcol
    echo -n $git_branch
    set_color $textcol -b $bgcol
    echo -n ') $ '
  else
    echo -n (hostname)':'(prompt_pwd) '$ '
  end
end

