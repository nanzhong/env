FROM nanzhong/emacs:latest AS emacs
FROM nanzhong/tmux:2.9a AS tmux

FROM debian:unstable
RUN apt-get update && apt-get upgrade -qy && apt-get -qy install \
    software-properties-common automake pkg-config build-essential \
    locales \
    ca-certificates \
    htop iproute2 dnsutils \
    gnupg \
    fish rsync curl wget git bzr mercurial \
    jq direnv unzip  git-crypt \
    ispell \
    mysql-client \
    golang python lua5.3

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LC_CTYPE=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian buster stable"
RUN apt-get update && \
    apt-get -qy install \
            docker-ce

RUN curl -sLo docker-compose "https://github.com/docker/compose/releases/download/1.25.1/docker-compose-Linux-x86_64" && chmod +x docker-compose && mv docker-compose /usr/local/bin/docker-compose
RUN curl -sLO https://storage.googleapis.com/kubernetes-release/release/v1.17.2/bin/linux/amd64/kubectl && chmod +x kubectl && mv kubectl /usr/local/bin/kubectl
RUN curl -sL https://get.helm.sh/helm-v3.0.0-linux-amd64.tar.gz | tar -zvx --strip-components=1 -C /usr/local/bin linux-amd64/helm
RUN curl -sLo fly https://github.com/concourse/concourse/releases/download/v5.8.0/fly_linux_amd64 && chmod +x fly && mv fly /usr/local/bin/.
RUN curl -sLO https://prerelease.keybase.io/keybase_amd64.deb && apt-get -y install ./keybase_amd64.deb && rm keybase_amd64.deb

COPY --from=emacs /tmp/emacs /tmp/emacs
RUN rsync -a /tmp/emacs/ / && rm -rf /tmp/nemacs

COPY --from=tmux /tmp/tmux /tmp/tmux
RUN rsync -a /tmp/tmux/ / && rm -rf /tmp/tmux

RUN mkdir -p /root/bin
ENV PATH="/root/bin:${PATH}"

RUN git clone https://github.com/nanzhong/dotfiles.git /root/dotfiles
RUN cd /root/dotfiles && git remote set-url origin git@github.com:nanzhong/dotfiles.git
RUN ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf
RUN ln -s ~/dotfiles/.config ~/.config
RUN ln -s ~/dotfiles/.gitconfig ~/.gitconfig
RUN ln -s ~/dotfiles/.gitignore ~/.gitignore
RUN ln -s ~/dotfiles/.terminfo ~/.terminfo
RUN ln -s ~/dotfiles/bin/cpcat ~/bin/cpcat

RUN curl -fsSL https://starship.rs/install.sh | bash
RUN echo "starship init fish | source" > ~/.config/fish/conf.d/starship.fish

RUN git clone https://github.com/nanzhong/emacs-nan-theme.git /root/dotfiles/.config/emacs/emacs-nan-theme
RUN cd /root/.config/emacs/emacs-nan-theme && git remote set-url origin git@github.com:nanzhong/emacs-nan-theme.git

RUN /usr/local/bin/emacs --batch --load /root/.config/emacs/early-init.el --load /root/.config/emacs/init.el --eval '(kill-emacs)'

RUN git clone https://github.com/junegunn/fzf /root/.fzf
RUN cd /root/.fzf && git remote set-url origin git@github.com:junegunn/fzf.git
RUN /root/.fzf/install --bin --64 --no-bash --no-zsh --no-fish
RUN cp /root/.fzf/shell/key-bindings.fish ~/.config/fish/functions/fzf_key_bindings.fish

RUN git clone https://github.com/skywind3000/z.lua /root/.z.lua
RUN cd /root/.z.lua && git remote set-url origin git@github.com:skywind3000/z.lua.git
RUN lua5.3 /root/.z.lua/z.lua --init fish > ~/.config/fish/conf.d/z.fish

RUN go get golang.org/dl/go1.13 && /root/go/bin/go1.13 download && ln -s /root/go/bin/go1.13 /root/bin/go
RUN go get -u github.com/mdempsky/gocode
RUN go get -u golang.org/x/tools/cmd/...
RUN go get -u github.com/aybabtme/humanlog/cmd/...
RUN GO111MODULE=on go get golang.org/x/tools/gopls@latest

RUN git clone https://github.com/rbenv/rbenv.git ~/.rbenv
RUN mkdir -p ~/.rbenv/plugins
RUN git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
RUN apt-get install -qy libssl-dev libreadline-dev zlib1g-dev
RUN fish -c "rbenv install 2.7.0 && rbenv global 2.7.0"

RUN curl -sL https://deb.nodesource.com/setup_13.x | bash -
RUN apt-get install -y nodejs

RUN chsh -s /usr/bin/fish
# allow keybase to be used
ENV KEYBASE_ALLOW_ROOT 1
# for correct colours in tmux
ENV TERM xterm-256color

COPY session-init.sh /bin/session-init
CMD ["/bin/session-init"]

WORKDIR /root