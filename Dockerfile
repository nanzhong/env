FROM ubuntu:18.04

RUN apt-get update && apt-get upgrade -qy
RUN apt-get -qy install \
    software-properties-common \
    automake pkg-config build-essential \
    ca-certificates \
    gnupg-agent \
    locales \
    curl wget git

RUN add-apt-repository ppa:kelleyk/emacs
RUN apt-add-repository ppa:fish-shell/release-3
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"

RUN apt-get update && apt-get -qy install \
    locales \
    fish jq direnv unzip htop iproute2 dnsutils git-crypt \
    ispell mysql-client \
    emacs26 \
    golang python lua5.3 \
    docker-ce

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LC_CTYPE=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# build tmux from source for 2.9a
RUN apt-get -qy install \
    libevent-dev \
    libncurses-dev
RUN curl -sLO https://github.com/tmux/tmux/releases/download/2.9a/tmux-2.9a.tar.gz && \
    tar xzvf tmux-2.9a.tar.gz && \
    cd tmux-2.9a && \
    ./configure && make install && \
    cd ../ && rm -rf tmux-2.9a*
# for correct colours in tmux
ENV TERM xterm-256color

RUN chsh -s /usr/bin/fish
RUN mkdir -p /root /root/bin /root/go/bin
ENV PATH="/root/bin:${PATH}"

RUN curl -sLO https://storage.googleapis.com/kubernetes-release/release/v1.14.3/bin/linux/amd64/kubectl && chmod +x kubectl && mv kubectl /usr/local/bin/kubectl

ENV KEYBASE_ALLOW_ROOT 1
RUN curl -sLO https://prerelease.keybase.io/keybase_amd64.deb && apt-get -y install ./keybase_amd64.deb && rm keybase_amd64.deb

RUN curl -sLo fly https://github.com/concourse/concourse/releases/download/v4.2.3/fly_linux_amd64 && chmod +x fly && mv fly /usr/local/bin/.

RUN git clone https://github.com/nanzhong/dotfiles.git /root/dotfiles
RUN ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf
RUN ln -s ~/dotfiles/.config ~/.config
RUN ln -s ~/dotfiles/.emacs.d ~/.emacs.d
RUN ln -s ~/dotfiles/.gitconfig ~/.gitconfig
RUN ln -s ~/dotfiles/.gitignore ~/.gitignore
RUN cd /root/dotfiles && git remote set-url origin git@github.com:nanzhong/dotfiles.git

RUN git clone https://github.com/nanzhong/emacs-nan-theme.git /root/.emacs.d/emacs-nan-theme
RUN cd /root/.emacs.d/emacs-nan-theme && git remote set-url origin git@github.com:nanzhong/emacs-nan-theme.git

RUN emacs --batch --load /root/.emacs.d/init.el --eval '(kill-emacs)'

RUN git clone https://github.com/junegunn/fzf /root/.fzf
RUN cd /root/.fzf && git remote set-url origin git@github.com:junegunn/fzf.git
RUN /root/.fzf/install --bin --64 --no-bash --no-zsh --no-fish
RUN cp /root/.fzf/shell/key-bindings.fish ~/.config/fish/functions/fzf_key_bindings.fish

RUN git clone https://github.com/skywind3000/z.lua /root/.z.lua
RUN cd /root/.z.lua && git remote set-url origin git@github.com:skywind3000/z.lua.git
RUN lua5.3 /root/.z.lua/z.lua --init fish > ~/.config/fish/conf.d/z.fish

RUN fish -c "go get golang.org/dl/go1.12.4 && go1.12.4 download && ln -s (which go1.12.4) /root/bin/go"
RUN fish -c "go get -u github.com/mdempsky/gocode"
RUN fish -c "go get -u golang.org/x/tools/cmd/..."
RUN fish -c "go get -u github.com/aybabtme/humanlog/cmd/..."

# until saibing's changes are included upstream...
RUN fish -c "git clone -b bingo https://github.com/saibing/tools.git /tmp/tools && cd /tmp/tools/cmd/gopls && go install && cd / && rm -rf /tmp/tools"

RUN git clone https://github.com/rbenv/rbenv.git ~/.rbenv
RUN mkdir -p ~/.rbenv/plugins
RUN git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
RUN apt-get install -qy libssl-dev libreadline-dev zlib1g-dev
RUN fish -c "rbenv install 2.6.3 && rbenv global 2.6.3"

COPY session-init.sh /bin/session-init
CMD ["/bin/session-init"]

WORKDIR /root