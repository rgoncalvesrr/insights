# Insights
Ideias, boas práticas, memória e algo que não me lembro agora... 😁

[Mode-View-Presenter Delphi](mvp.md) | [Meu Linux Desktop](meu-linux.md)

## Organização dos módulos

A sugestão que mais me agrada, apesar de gerar muito código, é a organização nos níveis:

- **Nível 1**, nome da empresa ou produto. Exemplo:

  ```
  com.conceihto
  ```
  
  
- **Nível 2**, contexto delimitado. Exemplo:

  ```
  com.conceitho.certificados
  
  com.conceitho.faturamento
  ```
  
- ***Nível 3***, natureza do código expressado. Exemplo:

  ```
  com.conceitho.certificados.api
  
  com.conceitho.certificados.domain

  com.conceitho.certificados.infraestructure
  ```
  
- ***Nível 4 (opcional)***, divisão entre modelo e serviços. Exemplo:

  ```
  com.conceitho.certificados.api
  
  com.conceitho.certificados.domain.model
  
  com.conceitho.certificados.domain.services

  com.conceitho.certificados.infraestructure
  ```

O modelo privilegia os negócio em detrimento da tecnologia. Nas divisões de nível mais alto ficam o contexto e, nas inferiores, a tecnologia;
Aqui, o negócio brilha e não a tecnologia.

Tendo em mente que queremos evitar que a camada de aplicação possa referênciar a camada de infraestrutura, temos que garantir que todos os artefatos
de infraestrutura sejam privados, isto é, nossa organização deve expor somente as interfaces de serviços. Mas... como fazer isso?!

A seguir, um sugestão de como organizar um projeto em dotnet.
  
## Modelo de Organização da Solução em Dotnet

Aqui estamos assumindo uma aplição de backend, então, com isso em mente, nossa solução será composta por 2 projetos apenas:
- Aplicação (API)
- Core (Aplicação)

> [!IMPORTANT]
>  SIM, a "sujeira" ficará dentro do módulo `Core`. Tenha calma, será uma "sujeira" organizada.

Veja o esquema a seguir:


![image](https://github.com/user-attachments/assets/86475b25-be38-4087-9d48-4ca7b04e9aa5)