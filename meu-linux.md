# Roteiro de configuração do linux

- [Opções do Kernel](#parâmetros-do-kernel)
  - [NVidia DRM](#nvidia-drmmodeset1)
  - [Intel IOMMU](#intel_iommuon)
  - [Pass-Through Memória Virtualização](#iommupt)
  - [Desabilitar AppArmor](#apparmor0)
- [Agendado de Disco](#agendador-de-disco-scheduler)
- [Ajeitando o Cedilha](#ajeitando-o-cedilha-errado-ć-no-debian-e-derivados)

> [!IMPORTANT]
> Essas configurações foram criadas para o hardware a seguir:
> 
> ![image](https://github.com/user-attachments/assets/ca11ed71-9420-4e01-a984-72f14cfe5632)
> 

## Parâmetros do Kernel

Arquivo `/boot/efi/loader/entries/Pop_OS-current.conf`

```bash
title Pop!_OS
linux /EFI/Pop_OS-5c0bd230-805d-4fec-943c-f8d5ba8a5911/vmlinuz.efi
initrd /EFI/Pop_OS-5c0bd230-805d-4fec-943c-f8d5ba8a5911/initrd.img
options root=UUID=5c0bd230-805d-4fec-943c-f8d5ba8a5911 ro quiet loglevel=0 systemd.show_status=false splash nvidia-drm.modeset=1 intel_iommu=on iommu=pt apparmor=0
```

[NVidia DRM](#nvidia-drmmodeset1) |
[Intel IOMMU](#intel_iommuon) | 
[Pass-Through Memória Virtualização](#iommupt) |
[Desabilitar AppArmor](#apparmor0)

### `nvidia-drm.modeset=1`

- **Para que serve**: Habilita o modsetting do kernel (Kernel ModeSetting - KMS) para drivers da NVIDIA.
- **Detalhes**: O KMS permite que o kernel configure os modos de exibição (resolução, profundidade de cor, etc.) diretamente, antes mesmo que o ambiente gráfico seja carregado. Com `nvidia-drm.modeset=1`, o driver proprietário da NVIDIA pode aproveitar essa funcionalidade, o que geralmente resulta em:
  - Transição mais suave da tela de boot para o ambiente gráfico.
  - Melhor compatibilidade e desempenho para algumas GPUs NVIDIA.
  - Solução de problemas em certas configurações de vídeo.
   
### `intel_iommu=on`

  - **Para que serve**: Habilita a funcionalidade de **Intel IOMMU (Input/Output Memory Management Unit)**.
  - **Detalhes**: O IOMMU é uma unidade de gerenciamento de memória que permite que dispositivos de E/S (como placas de vídeo, placas de rede, etc.) acessem a memória do sistema de forma mais segura e eficiente. É crucial para tecnologias como a virtualização VT-d da Intel, pois permite que máquinas virtuais acessem hardware diretamente com isolamento de memória.
 
### `iommu=pt`

- **Para que serve**: Configura o modo de operação do IOMMU para "pass-through".
- **Detalhes**: Quando usado em conjunto com `intel_iommu=on`, `iommu=pt` (pass-through) é essencial para cenários de virtualização onde você deseja que uma máquina virtual tenha acesso direto e exclusivo a um dispositivo de hardware físico (por exemplo, uma placa de vídeo dedicada). Ele garante que o IOMMU isole o dispositivo para a VM, evitando conflitos e melhorando o desempenho.

### `apparmor=0`

- **Para que serve**: Desabilita o AppArmor.
- **Detalhes**: O AppArmor é um **Módulo de Segurança do Linux (LSM)** que impõe perfis de segurança para programas individuais, restringindo seus recursos e acesso a arquivos e rede. Ao definir `apparmor=0`, você desativa completamente essa camada de segurança. Isso pode ser feito para depuração de problemas, ou em ambientes onde o AppArmor não é necessário ou interfere com alguma aplicação específica, mas é importante notar que isso reduz a segurança do sistema.

> [!IMPORTANT]
>
> Esse parâmetro é válido para distribuições baseadas em Debian que utilizam **AppArmor** em detrimento do **SELinux**
> 


### `selinux=0`

- **Para que serve**: Desabilita o Selinux.
- **Detalhes**: O SELinux é um **Módulo de Segurança do Linux (LSM)** que impõe perfis de segurança para programas individuais, restringindo seus recursos e acesso a arquivos e rede. Ao definir `selinux=0`, você desativa completamente essa camada de segurança. Isso pode ser feito para depuração de problemas, ou em ambientes onde o SELinux não é necessário ou interfere com alguma aplicação específica, mas é importante notar que isso reduz a segurança do sistema.

> [!IMPORTANT]
>
> Esse parâmetro é válido para distribuições baseadas em Red Hat que utilizam **SELinux** em detrimento do **AppArmor**
> 


## Agendador de Disco (Scheduler)


- [Passo 1: Verificar os agendadores disponíveis](#passo-1-verificar-os-agendadores-disponíveis) 
- [Passo 2: Editar o arquivo de configuração do GRUB](#passo-2-editar-o-arquivo-de-configuração-do-grub) 
- [Passo 3: Atualizar o GRUB](#passo-3-atualizar-o-grub) 
- [Passo 4: Reiniciar o sistema](#passo-4-reiniciar-o-sistema) 
- [Passo 5: Verificar se o Kyber está ativo](#passo-5-verificar-se-o-kyber-está-ativo) 
- [Considerações adicionais](#considerações-adicionais)

### Passo 1: Verificar os agendadores disponíveis

Primeiro, verifique quais agendadores de E/S estão disponíveis para seus dispositivos de bloco. Abra um terminal e execute o seguinte comando para um de seus discos (substitua sda pelo nome do seu disco, como nvme0n1 para NVMe):

```bash
cat /sys/block/sda/queue/scheduler
```

Você verá uma lista de agendadores entre colchetes, e o agendador atualmente ativo estará entre parênteses. Por exemplo: `[mq-deadline] kyber bfq none`.

### Passo 2: Editar o arquivo de configuração do GRUB

Você precisará editar o arquivo `/etc/default/grub` com privilégios de superusuário.

```bash
sudo nano /etc/default/grub
```

Procure a linha que começa com `GRUB_CMDLINE_LINUX_DEFAULT`. Ela pode ser algo como:

```bash
GRUB_CMDLINE_LINUX_DEFAULT="rhgb quiet"
```

Para habilitar o Kyber, adicione `elevator=kyber` a esta linha. Ficará assim:

```bash
GRUB_CMDLINE_LINUX_DEFAULT="rhgb quiet elevator=kyber"
```

_**Importante**: Se você estiver usando um SSD NVMe moderno, o agendador "none" (nenhum) é frequentemente o mais recomendado, pois esses dispositivos já possuem um bom agendamento interno e a adição de um agendador de software pode, na verdade, diminuir o desempenho. O Kyber é mais indicado para HDDs tradicionais ou SSDs SATA mais antigos._

### Passo 3: Atualizar o GRUB

Após salvar o arquivo `/etc/default/grub`, você precisa atualizar a configuração do GRUB para que as alterações entrem em vigor. No Fedora, o comando é:

```bash
sudo grub2-mkconfig -o /boot/grub2/grub.cfg
```

Se você usa UEFI, o caminho pode ser ligeiramente diferente:

```bash
sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg
```

### Passo 4: Reiniciar o sistema

Para que as mudanças sejam aplicadas, você precisa reiniciar o seu computador:

```Bash
sudo reboot
```

### Passo 5: Verificar se o Kyber está ativo

Após reiniciar, verifique novamente o agendador de E/S para confirmar que o Kyber está ativo:

```Bash
cat /sys/block/sda/queue/scheduler
```

Agora, Kyber deverá estar entre parênteses, indicando que está em uso: `mq-deadline [kyber] bfq none`.

## Ajeitando o cedilha errado (ć) no Debian e derivados

Ajeitar o cedilha errado no Debian e derivados usando Gnome.

### Passo 1: **Confirme que o layout (Fonte de entrada) do seu teclado possui “intern.” ou “intl.” no nome**
  
  Exemplos: Ingês (EUA, intern. alt.) e English (US, intl., with dead keys). Esta informação está na tela de configuração do teclado do sistema.

### Passo 2: **Edite o arquivo /etc/environment**

  A maneira mais fácil para editar este arquivo é abrir um terminal e digitar:
  ```bash
  sudo gnome-text-editor /etc/environment
  ``` 

  OU
  
  ```bash
  sudo vim /etc/environment
  ```

  O comando acima vai pedir a senha do seu usuário e depois de digitá-la (enquanto você digita o terminal não irá mostrar nada) é só apertar a tecla `ENTER` que o editor gedit vai abrir. Não tem problema se o seu arquivo estiver vazio.
  
### Passo 3: **Adicionar linhas no final do arquivo**

  Adicione as seguintes linhas no final do arquivo:
  
  ``` 
  GTK_IM_MODULE=cedilla
  QT_IM_MODULE=cedilla
  ```

### Passo 4: **Salve o arquivo**

  Clique no botão ” Salvar” localizado no canto direito superior da tela, ou use a tecla de atalho `Shift+:`, `x`, `Enter`.

### Passo 5: **Encerre a sessão do seu usuário ou reinice o computador**
  
  É necessário deslogar e logar novamente para a alteração funcionar. Caso não funcione, reinicie seu computador.

Pronto, agora você pode escrever o ç corretamente!

### **Solução para aplicativos GTK 4**

Para aplicativos GTK 4 (como o gnome-text-editor que veio no Ubuntu 24.04) é necessário fazer os passos abaixo.

- Criar um arquivo .XCompose no diretório do seu usuário com o conteúdo abaixo:

  ```bash
  # UTF-8 (Unicode) compose sequences

   # Overrides C acute with Ccedilla:
   <dead_acute> <C> : "Ç" "Ccedilla"
   <dead_acute> <c> : "ç" "ccedilla"
  ```

- Executar o comando no terminal:

  ```bash
  gsettings set org.gnome.settings-daemon.plugins.xsettings overrides "{'Gtk/IMModule': <'ibus'>}"
  ```

  _**Fonte: [Daniel Kossmann](https://www.danielkossmann.com/pt/ajeitando-cedilha-errado-ubuntu-linux/) acessado em 29/8/2025**_

### Considerações adicionais:

- **Tipos de disco**: Como mencionado, o agendador ideal depende do tipo de armazenamento. Para NVMe SSDs, `"none"` geralmente é o melhor. Para HDDs ou SSDs SATA mais antigos, Kyber ou BFQ podem oferecer melhor desempenho em certas cargas de trabalho.
- **TuneD**: O Fedora (e Red Hat Enterprise Linux) utiliza o TuneD para otimização de desempenho. Você pode usar o TuneD para definir o agendador de disco para perfis específicos. No entanto, a modificação do GRUB é uma maneira mais direta de definir o agendador padrão para todos os discos.
- **Regras udev**: Para um controle mais granular sobre agendadores de disco por dispositivo, você pode usar regras udev. No entanto, para a maioria dos usuários, definir no GRUB é suficiente.

## Instalação de Pacotes

### Fedora 42

#### Ferramentas de Desenvolvimento

```bash
dnf group install -y development-tools
```

```bash
dnf install -y golang java-21-openjdk java-21-openjdk-devel dotnet-sdk-8.0 nodejs
```

#### Escritório

```bash
dnf install -y thunderbird vim htop duf fastfetch gnucash libdbi-dbd-sqlite
```
