function tmux-status-minikube
    set minikube_ip (minikube ip ^ /dev/null)
    if test -z $minikube_ip
        set minikube_ip ...
    end
    echo "歷 $minikube_ip"
end
