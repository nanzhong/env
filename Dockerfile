FROM archlinux:latest

ENV MAKEFLAGS="-j$(nproc)"
# RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
# RUN locale-gen en_US.UTF-8
# ENV LANG=en_US.UTF-8
# ENV LC_CTYPE=en_US.UTF-8
# ENV LC_ALL=en_US.UTF-8
# # for correct colours in tmux
# ENV TERM xterm-24bit

# Upgrade and install base dependencies
RUN pacman -Syyu --noconfirm \
    base-devel \
    docker sudo openssh \
    iproute dnsutils inetutils htop \
    mariadb-clients \
    tmux fish curl wget rsync jq unzip ispell tree \
    git git-crypt bzr mercurial \
    go python lua

# Additional dependencies
RUN curl -sLo docker-compose "https://github.com/docker/compose/releases/download/1.25.4/docker-compose-Linux-x86_64" && chmod +x docker-compose && mv docker-compose /usr/local/bin/docker-compose
RUN curl -sLO https://storage.googleapis.com/kubernetes-release/release/v1.17.4/bin/linux/amd64/kubectl && chmod +x kubectl && mv kubectl /usr/local/bin/kubectl
RUN curl -sL https://get.helm.sh/helm-v3.1.2-linux-amd64.tar.gz | tar -zvx --strip-components=1 -C /usr/local/bin linux-amd64/helm
RUN curl -sLo fly https://github.com/concourse/concourse/releases/download/v5.8.0/fly_linux_amd64 && chmod +x fly && mv fly /usr/local/bin/.

# Run as non-root user
RUN useradd -m -U -s /usr/bin/fish -G adm,log,wheel nan
RUN echo '%wheel ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER nan

# Install AUR dpendencies
RUN git clone https://aur.archlinux.org/yay.git /tmp/yay && \
    cd /tmp/yay && \
    makepkg -si --noconfirm && \
    rm -rf /tmp/yay
RUN yay -S --noconfirm direnv
RUN yay -S --noconfirm keybase-bin
RUN git clone https://aur.archlinux.org/emacs-git.git /tmp/emacs && \
    cd /tmp/emacs && \
    sed -i 's/CLI=/CLI="YES"/' PKGBUILD && \
    sed -i 's/NOTKIT=/NOTKIT="YES"/' PKGBUILD && \
    sed -i 's/CAIRO="YES"/NOX="YES"/' PKGBUILD && \
    makepkg -si --noconfirm && \
    rm -rf /tmp/emacs

# Create local bin path
RUN mkdir -p ~/bin

# Dotfiles
## Clone and link dotfiles repo
RUN rm -rf ~/.config
RUN git clone https://github.com/nanzhong/dotfiles.git ~/dotfiles
RUN cd ~/dotfiles && git remote set-url origin git@github.com:nanzhong/dotfiles.git
RUN ln -s ~/dotfiles/.config ~/.config
RUN ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf
RUN ln -s ~/dotfiles/.gitconfig ~/.gitconfig
RUN ln -s ~/dotfiles/.gitignore ~/.gitignore
RUN ln -s ~/dotfiles/.terminfo ~/.terminfo
RUN sudo ln -s ~/dotfiles/bin/cpcat /usr/local/bin/cpcat

## Configure prompt
RUN ls -la ~/
RUN curl -fsSL https://starship.rs/install.sh | bash -s -- --yes
RUN echo "starship init fish | source" > ~/.config/fish/conf.d/starship.fish

## Configure fzf
RUN git clone https://github.com/junegunn/fzf ~/.fzf
RUN cd ~/.fzf && git remote set-url origin git@github.com:junegunn/fzf.git
RUN ~/.fzf/install --bin --64 --no-bash --no-zsh --no-fish
RUN cp ~/.fzf/shell/key-bindings.fish ~/.config/fish/functions/fzf_key_bindings.fish

## Configure z
RUN git clone https://github.com/skywind3000/z.lua ~/.z.lua
RUN cd ~/.z.lua && git remote set-url origin git@github.com:skywind3000/z.lua.git
RUN lua ~/.z.lua/z.lua --init fish > ~/.config/fish/conf.d/z.fish

## Configure emacs
RUN git clone https://github.com/nanzhong/emacs-nan-theme.git ~/dotfiles/.config/emacs/emacs-nan-theme
RUN cd ~/.config/emacs/emacs-nan-theme && git remote set-url origin git@github.com:nanzhong/emacs-nan-theme.git
RUN emacs --batch --load ~/.config/emacs/early-init.el --load ~/.config/emacs/init.el --eval '(kill-emacs)'

# Programming languages
## Go
RUN go get -u github.com/mdempsky/gocode
RUN go get -u golang.org/x/tools/cmd/...
RUN go get -u github.com/aybabtme/humanlog/cmd/...
RUN GO111MODULE=on go get golang.org/x/tools/gopls@latest

## Ruby
RUN git clone https://github.com/rbenv/rbenv.git ~/.rbenv
RUN mkdir -p ~/.rbenv/plugins
RUN git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
RUN fish -c 'rbenv install 2.7.0 && rbenv global 2.7.0'

## NodeJS
RUN curl -L https://git.io/n-install | N_PREFIX=~/.n bash -s -- -yn lts
RUN PATH=~/.n/bin:$PATH npm install -g typescript typescript-language-server

COPY session-init.sh /bin/session-init
CMD ["/bin/session-init"]

WORKDIR /home/nan
