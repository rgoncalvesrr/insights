# Documentação de Arquitetura de Software: Padrão MVP em Delphi

| Versão | Data | Autor |
|-|-|-|
| 1.0 | 26 de junho de 2025 | Gemini |
| 1.1 | 26 de junho de 2025 | Ricardo Gonçalves |

## 1. Objetivo
Este documento estabelece a arquitetura padrão para o desenvolvimento de novas funcionalidades do sistema. O objetivo principal é resolver problemas comuns em aplicações VCL legadas, como o acoplamento de código, a mistura de responsabilidades (UI, regras de negócio, acesso a dados) e a baixa testabilidade.

A arquitetura adotada é a **Model-View-Presenter (MVP)**, que promove uma clara separação de camadas, resultando em um software mais robusto, manutenível, testável e flexível.

## 2. Princípios Fundamentais
A arquitetura se baseia nos seguintes princípios de engenharia de software:

- **Separação de Responsabilidades (SoC)**: Cada classe ou camada tem uma única responsabilidade. A interface do usuário (View) não conhece as regras de negócio; o serviço de negócio (Model/Service) não conhece a UI; e a camada de dados não conhece nenhuma das duas.

- **Baixo Acoplamento e Alta Coesão**: Os componentes são o mais independentes possível (baixo acoplamento). As funcionalidades relacionadas estão agrupadas de forma lógica (alta coesão).

- **Injeção de Dependência (DI)**: As dependências (como um serviço de log ou de acesso a dados) não são criadas dentro das classes que as utilizam. Em vez disso, são "injetadas" de fora, geralmente via construtor. Isso é facilitado pelo uso de interfaces.

- **Testabilidade**: A separação da lógica de negócio da UI permite a criação de testes de unidade (DUnit) para validar as regras do sistema sem a necessidade de interação manual com as telas.

## 3. Visão Geral da Arquitetura (MVP)
O padrão Model-View-Presenter divide a funcionalidade em três papéis distintos:

- **Model (Modelo)**: Representa a camada de dados e de lógica de negócio. Em nossa implementação, ela é dividida em:
  - **Objetos de Domínio** (ex: `TCliente`): Classes simples que carregam os dados.
  - **Serviços** (Service) (ex: `TClienteService`): Contêm as regras de negócio, validações e orquestram as operações.  
  - **Acesso a Dados** (Data Access) (ex: `TDMClientes`): Responsável exclusivamente pela comunicação com o banco de dados.

- **View (Visão)**: A interface do usuário (ex: `TFormPesquisaClientes`). É "passiva" e "burra". Suas únicas responsabilidades são exibir dados e capturar eventos do usuário (clicks, digitação), delegando qualquer processamento ao Presenter.

- **Presenter (Apresentador)**: O cérebro da operação. Atua como um intermediário que conecta a View ao Model. Ele recebe os eventos da View, aciona a lógica no Service, recebe os dados de volta e formata-os para serem exibidos na View.

### Diagrama de Componentes
Este diagrama ilustra a relação estática entre os principais componentes do nosso exemplo de "Cadastro de Clientes".

![image](./assets/mvp-diagrama-comp.png)

## 4. Detalhamento dos Componentes

### 4.1. Interfaces (`Cliente.Interfaces.pas`, `Logger.Interfaces.pas`)
As interfaces são o contrato que garante o baixo acoplamento.

- `ILogger`: Define o que um serviço de log deve fazer (`LogInfo`, `LogError`), mas não como. Permite trocar um log de arquivo por um de banco de dados sem alterar nenhuma outra parte do sistema.
- `IClienteView`, `IClientePesquisaView`: Definem as propriedades e métodos que a View **deve** expor para o Presenter (ex: `GetNome`, `SetNome`, `GetDataSource`). Isso permite que o Presenter trabalhe com a View sem conhecer sua classe concreta (`TForm`), o que é essencial para testes.
- `IClientePresenter`, `IClientePesquisaPresenter`: Definem as ações que a View pode solicitar ao Presenter (ex: `Salvar`, `Pesquisar`).

### 4.2. A Camada de Apresentação (View)
- **Responsabilidade**: Exibir dados e capturar interações do usuário.

- **Implementação**: TFormPesquisaClientes e TFormCadastroCliente.

- **Características**:
  - Implementa a interface de View correspondente.
  - Contém os componentes visuais (`TDBGrid`, `TEdit`, `TButton`).
  - **Não contém nenhuma regra de negócio**.
  - Os eventos de clique dos botões são, em geral, uma única linha que chama o método correspondente no Presenter.
  - Exemplo:
    ```delphi
    procedure TFormPesquisaClientes.btnPesquisarClick(Sender: TObject);
    begin
      FPresenter.Pesquisar;
    end;
    ```


### 4.3. O Apresentador (Presenter)
- **Responsabilidade**: Orquestrar a comunicação entre a View e o Model.

- **Implementação**: `TClientePesquisaPresenter` e `TClientePresenter`.

- **Características**:
  - Recebe a interface da View (`I...View`) e a instância do Service no seu construtor.
  - Contém a lógica de apresentação: "Quando o usuário clicar em Editar, pegue o ID da View, chame o método da View para abrir a outra tela, passando o ID".
  - Recebe dados do Service e os envia para a View de forma simples para que ela possa exibi-los.


### 4.4. A Camada de Lógica (Service)
- **Responsabilidade**: Implementar e garantir as regras de negócio do sistema.

- **Implementação**: `TClienteService`.

- **Características**:
  - É uma classe `TObject` pura. Não conhece e não deve conhecer nenhuma classe da VCL.
  - Recebe suas dependências (Logger, Data Access) via construtor.
  - Contém os métodos centrais da funcionalidade (`Salvar`, `Carregar`, `Listar`).
  - É aqui que as validações ocorrem (ex: "O nome do cliente não pode ser vazio").
  - É aqui que a lógica de log é invocada.


### 4.5. A Camada de Dados (Data Access)
- **Responsabilidade**: Exclusivamente ler e escrever no banco de dados.

- **Implementação**: `TDMClientes` (um `TDataModule`).

- **Características**:
  - Centraliza todos os componentes de acesso a dados (`TADOConnection`, `TADOQuery`).
  - Expõe métodos de alto nível (ex: `CarregarPorID`, `Listar`).
  - Retorna ou um objeto de domínio preenchido (`TCliente`) ou um TDataSet para listagens, mas não contém lógica de negócio.


## 5. Fluxos de Trabalho em Ação

### 5.1. Fluxo: Listar Clientes na Grade
1. **Usuário** digita um termo na `edtBusca` e clica no botão `btnPesquisar`.

2. **View** (`TFormPesquisaClientes`) aciona o método `btnPesquisarClick`, que chama `FPresenter.Pesquisar`.

3. **Presenter** (`TClientePesquisaPresenter`) chama `FService.Listar`, passando o termo de busca que ele obteve da View.

4. **Service** (`TClienteService`) loga a ação e chama FClienteData.Listar.

5. **Data Access** (`TDMClientes`) executa a consulta `SELECT` no banco e retorna o `TADOQuery` (um `TDataSet`) aberto.

6. **Presenter** recebe o `TDataSet` e o atribui ao `DataSource` da View: `FView.DataSource.DataSet := LDataSet;`.

7. **View**, através da mágica data-aware da VCL, exibe automaticamente os dados na TDBGrid.


### 5.2. Fluxo: Editar um Cliente
1. **Usuário** dá um duplo-clique num registro da `TDBGrid`.

2. **View de Pesquisa** aciona o método `btnEditarClick`, que chama `FPresenter.Editar`.

3. **Presenter de Pesquisa** obtém o `IDSelecionado` da View e chama `FView.AbrirTelaCadastro(ID)`.

4. **View de Pesquisa** cria a instância do `TFormCadastroCliente` e passa o ID para ele.

5. **View de Cadastro**, ao ser exibida (`OnShow`), chama `FPresenter.CarregarDados`.

6. **Presenter de Cadastro** chama `FService.Carregar(ID)`. O Service busca os dados no banco (via DataModule) e retorna um objeto `TCliente` preenchido.

7. **Presenter de Cadastro** recebe o objeto `TCliente` e atualiza a View: `FView.Nome := LCliente.Nome`, `FView.Cidade := LCliente.Cidade`.

8. **View de Cadastro** implementa `SetNome`, que simplesmente faz `edtNome.Text := Value`.

9. **Usuário** altera os dados e clica em "Salvar". O fluxo se inverte, passando pelo Presenter, pelo Service (onde as regras são validadas e o log é gravado) e pelo Data Access para executar o `UPDATE` no banco.

10. **A View de Cadastro** é fechada.

11. O controle volta para a View de Pesquisa, que, para garantir que a grade está atualizada, chama `FPresenter.Pesquisar` novamente.

A decisão de **não reutilizar o DataSet** da grade na tela de edição é fundamental, pois garante o isolamento das telas e o respeito à camada de serviço.

**Diagrama de Sequência: Ação "Salvar"**

![Diagrama de Sequência](./assets/mvp-diagrama-seq.png)

## 6. Conclusão: Benefícios da Arquitetura Adotada
A aplicação deste padrão, embora exija uma estrutura de arquivos mais elaborada, traz benefícios imensuráveis a médio e longo prazo:

- **Manutenibilidade**: Encontrar e alterar uma regra de negócio é fácil e seguro, pois ela reside em um único local (o Service).
  
- **Testabilidade**: A lógica de negócio pode ser testada de forma automatizada, sem interação com a UI.
  
- **Reutilização**: O `TClienteService` pode ser reutilizado por outras partes do sistema (um web service, um relatório, uma importação em lote) sem alterações.

- **Flexibilidade**: É possível alterar a tecnologia de banco de dados (modificando apenas o DataModule), o mecanismo de log (criando um novo `ILogger`) ou até mesmo a biblioteca de UI com impacto mínimo nas regras de negócio.


# Comparação entre MVP e MVC

Esta documentação deve servir como a base para todo o desenvolvimento futuro, garantindo consistência e qualidade ao longo do ciclo de vida do software.

Ambos os padrões, MVC (Model-View-Controller) e MVP (Model-View-Presenter), buscam o mesmo objetivo fundamental: separar as responsabilidades em uma aplicação. No entanto, eles fazem isso de maneiras sutilmente diferentes, o que tem implicações importantes em testabilidade e acoplamento.

**O MVP é uma evolução e, portanto, mais recente que o MVC.**

- **MVC (Model-View-Controller)**: Surgiu na década de 1970 com a linguagem Smalltalk, sendo um dos padrões de arquitetura de UI mais antigos e influentes.

- **MVP (Model-View-Presenter)**: Foi introduzido na década de 1990 como uma especialização do MVC, com o objetivo de melhorar a separação de interesses e facilitar os testes automatizados, especialmente em aplicações com interfaces de usuário complexas.

## Principais Diferenças entre MVC e MVP
A diferença crucial está no papel do intermediário (Controller vs. Presenter) e em como ele interage com a View.

1. **Comunicação e Fluxo de Dados**
    - **MVC (Triangular)**: O `Controller` é o ponto de entrada. Ele recebe a entrada do usuário, manipula o `Model` e, em seguida, seleciona uma View para ser renderizada. A `View`, por sua vez, lê os dados diretamente do `Model` para se exibir. Isso cria uma dependência direta da `View` com o `Model`.
      - **Fluxo**: Usuário interage com a `View -> View` chama o `Controller -> Controller` atualiza o `Model -> Model` notifica a `View -> View` lê os dados do Model e se atualiza.

    - **MVP (Linear)**: O `Presenter` atua como um mediador completo entre o `Model` e a `View`. A `View` delega todas as interações do usuário para o `Presenter`. O `Presenter` então busca os dados do `Model`, aplica a lógica necessária e chama métodos específicos na `View` para que ela se atualize. A `View` não sabe da existência do `Model`.
      - **Fluxo**: Usuário interage com a `View -> View notifica o Presenter -> Presenter` obtém dados do `Model -> Presenter` formata os dados e atualiza a `View`.

2. **Acoplamento e Papel da View**
    - **MVC**: A `View` é acoplada ao `Model`. Ela precisa "saber" como ler e interpretar os dados do `Model`. Isso pode dificultar a troca do `Model` ou o teste da `View` de forma isolada.

    - **MVP**: A `View` é completamente desacoplada do `Model`. Ela é uma camada "burra" e passiva, que geralmente implementa uma interface com métodos como `setNomeUsuario(string nome)` ou `exibirMensagemErro(string msg)`. O `Presenter` é quem comanda a `View`.

3. **Testabilidade**
    - **MVC**: Pode ser mais difícil de testar. Como a `View` está ligada ao `Model`, testar a lógica do `Controller` muitas vezes exige a instanciação de componentes de UI ou a criação de Models complexos.

    - **MVP**: É altamente testável. Como a `View` é desacoplada e se comunica com o `Presenter` através de uma interface, é muito fácil criar uma "View Falsa" (mock) nos testes unitários para verificar se o `Presenter` está se comportando corretamente (ex: "quando eu chamo `presenter.salvar()`, o método `view.exibirSucesso()` é chamado?"). A lógica de apresentação, que reside toda no Presenter, pode ser testada sem qualquer dependência de frameworks de UI.

## Tabela Comparativa

| Característica | MVC (Model-View-Controller) | MVP (Model-View-Presenter) |
|-|-|-|
| **Padrão de Comunicação** | Triangular (Controller -> Model -> View) | Linear (View <-> Presenter <-> Model) |
| **Acoplamento** | A View é acoplada ao Model. | A View e o Model são desacoplados pelo Presenter. |
| **Papel da View** | Mais ativa. Lê dados diretamente do Model. | Passiva. Apenas exibe dados e delega eventos ao Presenter. |
| **Mediador** | `Controller` | `Presenter` |
| **Relação** | Um Controller pode gerenciar múltiplas Views. | Geralmente, um Presenter para cada View (relação 1:1). |
| **Testabilidade** | Moderada. Testar o Controller pode ser complexo. | Alta. O Presenter é facilmente testável com mocks da View. |
| **Origem** | Mais antigo (Década de 1970) | Mais recente (Década de 1990) |

| Model-View-Controller | Mode-View-Presenter |
|-|-|
| ![MVC](./assets/mvp-diagrama-mvc.png) | ![MVC](./assets/mvp-diagrama-mvp.png) |


## Diagrama de Sequência: MVC (Model-View-Controller)
Neste diagrama, a View tem conhecimento do Model e é responsável por buscar os dados para se renderizar depois que o Controller o atualiza.

![MVC](./assets/mvp-diagrama-mvc.png)

**Fluxo Detalhado (MVC)**:

1. O **Usuário** realiza uma ação na **View**.
2. A **View** invoca um método no **Controller**.
3. O **Controller** processa a lógica e atualiza o **Model**.
4. O **Model** notifica a **View** que seu estado foi alterado (usando o padrão Observer/Observable) ou o **Controller** devolve o controle à **View**.
5. A **View** busca _**(puxa)**_ os dados atualizados diretamente do **Model**.
6. A **View** se renderiza com os novos dados para o **Usuário**.


## Diagrama de Sequência: MVP (Model-View-Presenter)
Neste diagrama, a View é passiva. O Presenter atua como um intermediário completo, buscando os dados do Model e empurrando-os para a View.

![MVC](./assets/mvp-diagrama-mvp.png)

**Fluxo Detalhado (MVP)**:

1. O **Usuário** realiza uma ação na **View**.
2. A **View**, sendo passiva, apenas notifica o **Presenter** sobre o evento, sem nenhuma lógica.
3. O **Presenter** solicita os dados ao **Model**.
4. O **Model** retorna os dados solicitados para o **Presenter**.
5. O **Presenter** aplica qualquer lógica de formatação e empurra os dados para a **View**, chamando métodos específicos (ex: `view.setNome(...)`, `view.setEmail(...)`).
6. A **View** simplesmente atualiza seus componentes visuais com os dados que recebeu.
7. A **View** exibe o resultado para o **Usuário**.
