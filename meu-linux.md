# Roteiro de configuração do linux

- [Opções do Kernel](#parâmetros-do-kernel)
  - [NVidia DRM](#nvidia-drmmodeset1)
  - [Intel IOMMU](#intel_iommuon)
  - [Pass-Through Memória Virtualização](#iommupt)
  - [Desabilitar AppArmor](#apparmor0)


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
[Desabilitar AppArmor](#apparmor0) |

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
