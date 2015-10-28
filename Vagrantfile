Vagrant.configure(2) do |config|
  config.vm.box = "hashicorp/precise32"

  config.vm.provision "shell" do |s|
    s.path = "scripts/deploy.sh"
    s.privileged = false
    s.keep_color = true
  end
end
