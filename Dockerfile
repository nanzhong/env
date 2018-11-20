FROM debian:sid

RUN apt-get update && apt-get -qy upgrade
RUN apt-get -qy install \
    build-essential apt-transport-https ca-certificates curl gnupg2 software-properties-common locales tzdata \
    libssl-dev libreadline-dev zlib1g-dev \
    mosh tmux fish curl git jq direnv unzip htop dnsutils

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
RUN locale-gen en_US.UTF-8 
ENV LANG=en_US.UTF-8
ENV LC_CTYPE=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# for correct colours is tmux
ENV TERM screen-256color

# temporary until debian sid has emacs 26
RUN add-apt-repository "deb [arch=amd64] http://emacs.secretsauce.net unstable main"
RUN curl -fsSL http://emacs.secretsauce.net/key.gpg | apt-key add -
# use buster because no repo exists for sid
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian buster stable"
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add

RUN apt-get update && apt-get -y install emacs-snapshot docker-ce

RUN chsh -s /usr/bin/fish
RUN mkdir -p /root /root/src /root/bin /root/go/bin

RUN curl -sLO https://storage.googleapis.com/kubernetes-release/release/v1.12.2/bin/linux/amd64/kubectl && chmod +x kubectl && mv kubectl /usr/local/bin/kubectl

RUN curl -so op.zip https://cache.agilebits.com/dist/1P/op/pkg/v0.5.4/op_linux_amd64_v0.5.4.zip && unzip op.zip && mv op /root/bin/op && rm op.sig op.zip

RUN git clone https://github.com/nanzhong/dotfiles.git /root/src/dotfiles
RUN ln -s ~/src/dotfiles/.tmux.conf ~/.tmux.conf
RUN ln -s ~/src/dotfiles/.config ~/.config
RUN ln -s ~/src/dotfiles/.emacs.d ~/.emacs.d
RUN ln -s ~/src/dotfiles/.gitconfig ~/.gitconfig
RUN ln -s ~/src/dotfiles/.gitignore ~/.gitignore
RUN cd /root/src/dotfiles && git remote set-url origin git@github.com:nanzhong/dotfiles.git

RUN git clone https://github.com/nanzhong/emacs-nan-theme.git /root/src/emacs-nan-theme
RUN cd /root/src/emacs-nan-theme && git remote set-url origin git@github.com:nanzhong/emacs-nan-theme.git

RUN emacs --batch --load /root/.emacs.d/init.el --eval '(kill-emacs)'

RUN git clone https://github.com/junegunn/fzf /root/.fzf
RUN cd /root/.fzf && git remote set-url origin git@github.com:junegunn/fzf.git
RUN /root/.fzf/install --bin --64 --no-bash --no-zsh --no-fish

RUN git clone https://github.com/asdf-vm/asdf /root/.asdf
RUN cd /root/.asdf && git remote set-url origin git@github.com:asdf-vm/asdf.git
RUN /root/.asdf/bin/asdf plugin-add golang https://github.com/kennyp/asdf-golang.git
RUN /root/.asdf/bin/asdf install golang 1.11.1
RUN /root/.asdf/bin/asdf global golang 1.11.1
RUN /root/.asdf/bin/asdf plugin-add ruby https://github.com/asdf-vm/asdf-ruby.git
RUN /root/.asdf/bin/asdf install ruby 2.5.1
RUN /root/.asdf/bin/asdf global ruby 2.5.1

RUN fish -c "go get -u github.com/sourcegraph/go-langserver"
RUN fish -c "go get -u github.com/mdempsky/gocode"
RUN fish -c "go get -u golang.org/x/tools/cmd/..."
RUN fish -c "go get -u github.com/aybabtme/humanlog/cmd/..."

COPY session-init.sh /bin/session-init
CMD ["/bin/session-init"]

WORKDIR /root
