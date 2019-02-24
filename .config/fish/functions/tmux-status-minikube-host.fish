function tmux-status-minikube-host
    set minikube_ip (ssh 127.0.0.1 minikube ip 2> /dev/null)
    if test -z $minikube_ip
        set minikube_ip ...
    end
    echo "歷 $minikube_ip"
end
