# STICC - Manual Completo

**STICC** (Sistema Tutor Inteligente para Ciência da Computação).

Este documento é, ao mesmo tempo, um manual de uso do sistema e um **guia de Prolog** aplicado. Ele foi escrito para que você entenda cada caractere do código, mesmo que nunca tenha programado em lógica antes.

---

## 1. O Que é Este Projeto?

O STICC é um sistema especialista que atua como um guia automatizado. Diferente de softwares convencionais, ele não segue apenas uma sequência de instruções; ele possui uma base de conhecimento e regras lógicas para inferir novas informações.
1.  **Base de Conhecimento (`src/data.pl`):** Contém os fatos estáticos. O sistema "conhece" todas as disciplinas, tópicos detalhados e a árvore de pré-requisitos do curso.
2.  **Motor de Inferência (`src/rules.pl`):** A inteligência lógica. Ele deduz se um aluno está apto a cursar uma disciplina, o que falta aprender e quais são as dependências cruzadas.
3.  **Persistência e Controle (`src/persistence.pl` e `src/main.pl`):** Gerencia o ciclo de vida da aplicação, interação com usuário e salvamento de progresso em arquivos `.pl` (personas).

---

## 2. Instalação e Configuração

O Prolog é uma linguagem interpretada e não gera um executável `.exe` como C++ ou C#. O código roda sobre a máquina virtual (interpretador) do SWI-Prolog.

### Passo 1: Instalar o SWI-Prolog
O **SWI-Prolog** é o interpretador mais famoso e robusto para programação lógica.

1.  Acesse: [https://www.swi-prolog.org/download/stable](https://www.swi-prolog.org/download/stable)
2.  Baixe a versão correta para o seu sistema operacional.
3.  **⚠️ CRÍTICO (Windows):** Durante a instalação, uma caixa de diálogo perguntará sobre variáveis de ambiente.
    * Selecione a opção: **"Add swipl to the system PATH for all users"**.
	* *Motivo:* Sem isso, o terminal (PowerShell/CMD) não reconhecerá o comando `swipl` e o script de execução `run.ps1` falhará.

---

## 3. Como Rodar o Projeto

A organização de pastas é vital para que os módulos do Prolog se encontrem (via diretiva `ensure_loaded`).

```text
/ (Raiz do Projeto)
│   run.ps1            # Script de inicialização (Windows)
│   README.md          # Esta documentação
│
├── personas/          # Pasta de perfis (arquivos .pl gerados)
│       (Ex: usuario1.pl)
│
└── src/               # Código Fonte
    │   main.pl        # Ponto de Entrada e Loop Principal
    │   data.pl        # Banco de Dados (Fatos)
    │   rules.pl       # Regras Lógicas
    │   ui.pl          # Interface e Formatação
    │   handlers.pl    # Tratamento de Opções do Menu
    │   persistence.pl # Sistema de Arquivos
```

### No PowerShell

O arquivo `run.ps1` é um script PowerShell que limpa o console e monta o comando de carga correto. Na raiz do projeto, execute:

```PowerShell
.\run.ps1
```

### Manualmente (Linux, Mac ou Windows)

Caso prefira utilizar o comando direto para invocar o interpretador, carregar o código fonte e iniciar o predicado principal, abra o terminal na **pasta raíz** do projeto e digite:

```bash
swipl -s src/main.pl -g "iniciar, halt"
```

* `swipl`: Chama o interpretador.
* `-s src/main.pl`: Carrega o código-fonte ("source"). O `main.pl` carrega todo o resto automaticamente.
* `-g "iniciar, halt"`: Define o *goal* (objetivo) inicial. Executa o predicado `iniciar` automaticamente, seguido pelo predicado `halt` (para encerrar a sessão) ao terminar a execução do código.

-----

## 4. Prolog: Entendendo o Código

Prolog (*Programming in Logic*) é uma linguagem declarativa. Você não diz ao computador o fluxo passo-a-passo (como fazer um loop `for` ou `while`). Você diz *o que é verdade* e *quais são as regras*, e o computador decide como resolver o problema.

### Módulo 1: A Sintaxe Básica (Átomos, Variáveis e Fatos)

**Onde estudar:** `src/data.pl`

Antes de entender a lógica, é necessário distinguir os tipos de dados. O Prolog diferencia tipos pela capitalização (maiúscula/minúscula).

1.  **Átomos (Constantes):** Começam com letra **minúscula**. Representam um valor único e fixo.
      * Exemplos: `icc`, `cid1`, `si`.
2.  **Variáveis:** Começam com letra **Maiúscula** ou underscore (`_`). São "espaços vazios" que o Prolog tenta preencher através de unificação.
      * Exemplos: `Disciplina`, `ID`, `Lista`, `_Descricao`.
3.  **Strings:** Texto entre aspas simples (também são átomos).
      * Exemplos: `'Introdução à Programação'`.
4.  **Fatos:** A menor unidade de informação. São as verdades absolutas do banco de dados.
      * Sintaxe: `predicado(arg1, arg2, ..., argN).` (O ponto final é obrigatório).

Exemplo de fato:
```ProLog
disciplina(icc, 'Introdução à Ciência da Computação', 1).
```

Vamos dissecar essa linha:

1.  **Predicado (`disciplina`):** É o nome da relação. Pense como o nome de uma tabela em um banco de dados SQL.
2.  **Átomo (`icc`):** Qualquer palavra que começa com **letra minúscula** é um identificador único (como uma ID ou uma string fixa que não muda).
3.  **String (`'Introdução à Ciência da Computação'`):** Texto entre aspas simples.
4.  **Argumentos:** São os dados dentro dos parênteses. Aqui temos 3: Sigla, Nome e Período.
5.  **Ponto Final (`.`):** **Obrigatório**. É como o `;` do Java/C. Sem ele, o código não compila.

### Módulo 2: Estruturas Compostas e Listas

**Onde estudar:** `src/data.pl`

Em prolog, listas são usadas para agrupar dados. E esse projeto utiliza intensivamente **Listas** e **Pares**.

Exemplo:
```ProLog
lista_topicos(icc, [
	historia-'Geração Zero...',
	binario-'Sistemas de numeração...'
]).
```

1.  **Pares (Tuplas Improvisadas):** O código usa o operador hífen (`-`) para criar uma estrutura chave-valor.
	* Nesse caso: `historia-'Geração Zero...'`
	* Para o Prolog, isso é um dado único (Chave=`historia`, Valor=`'Geração Zero...'`). E é útil para associar um ID a uma descrição sem criar um novo predicado.

2.  **Listas:** Uma lista em Prolog é um conjunto de elementos. E diferente de linguagens como C, essas listas não precisam necessariamente conter elementos do mesmo tipo.
	* Sintaxe: `[elemento1, elemento2, ..., elementoN]`.
	* No caso do exemplo dado, é criada uma lista de `Pares`, onde cada elemento da lista é um par. Ex: `historia-'Geração Zero...'` é um único elemento dessa lista.

3.  **Iteração em listas (`[Cabeça|Cauda]`):**
    Prolog não possui laços `for` ou `while` nativos. A iteração é feita via recursão em listas.
	* Uma lista é dividida em **Head** (o primeiro item) e **Tail** (o resto da lista).
	* Sintaxe: `[H|T]`.

Exemplo de iteração no arquivo `ui.pl` (predicado `imprimir_lista_simples`):
```prolog
% 1. Caso Base: Se a lista for vazia, pare (não faça nada).
imprimir_lista_simples([]).

% 2. Passo Recursivo:
imprimir_lista_simples([Sigla-Nome | Resto]) :-
format('   [~w] ~w~n', [Sigla, Nome]),  % Processa a Cabeça (Head)
imprimir_lista_simples(Resto).          % Chama a função novamente para a Cauda (Tail)
```
* **O Operador `|` (Pipe):** Ele separa a cabeça (primeiro item) da cauda (resto da lista).
	* Se a lista é `[a, b, c]`:
	* `Sigla-Nome` vira `a`.
	* `Resto` vira `[b, c]`.

### Módulo 3: Regras e Unificação (O Motor de Inferência)

**Onde estudar:** `src/rules.pl`

Regras definem como inferir novas verdades a partir dos fatos existentes.
Sintaxe: `Conclusão :- Condição1, Condição2, ..., CondiçãoN.`

* `:-` lê-se **"É verdade SE..."**
* `,` (vírgula) significa **"E"**
* `;` (ponto e vírgula) significa **"OU"**
* `\+` (ponto e vírgula) significa **"NÃO"** (negação por falha)

**O Conceito de Unificação (Pattern Matching):**
O Prolog não atribui valores. Ele tenta "casar" padrões. Veja a regra `topico` em `rules.pl`:

```prolog
topico(Disciplina, ID) :-
	lista_topicos(Disciplina, Lista),
	member(ID-_Descricao, Lista).
```

**Tradução para Humanos:**
"O `ID` é um tópico da `Disciplina` **SE**:
1.  Existe uma `Lista` de tópicos associada a essa `Disciplina` no banco de dados, **E**...
2.  O `ID` é um membro (`member`) dessa `Lista`."

**O Conceito de Variável e Unificação:**
Note que `Disciplina`, `ID`, `Lista` começam com **Letra Maiúscula**.
* **Maiúscula = Variável:** É um espaço em branco. O Prolog tenta preencher.
* **Minúscula = Valor Fixo:** `icc`, `cid1`.

Se o programa executasse, por exemplo, a consulta `topico(icc, X).`:

1.  A variável `Disciplina` unifica (é substituída) com o átomo `icc`.
2.  O Prolog busca em `data.pl` o fato `lista_topicos(icc, Lista)` e "copia" a lista inteira para a variável `Lista`.
3.  Executa `member(ID-_Descricao, Lista)` que testa cada elemento da lista.
	*  **Variável Anônima (`_`):** Note o uso de `_Descricao`. O underscore diz ao Prolog: "Existe algo aqui, mas não importa o valor agora". O sistema extrai apenas o `ID` da lista de pares, ignorando a descrição textual.

### Módulo 4: O Motor de Busca e Meta-Predicados

**Onde estudar:** `src/rules.pl`

Para criar um sistema inteligente, é necessário manipular conjuntos de dados (ex: "todos os requisitos"). Para isso, podemos utilizamos alguns `Meta-Predicados`, que são **predicados de "ordem superior" (funções que rodam outras funções)**. No código, utilizamos três dessas ferramentas poderosas:

1.  **`forall(Condição, Ação)`:**
    Verifica se uma regra vale para *todos* os casos.

      * No código: `domina(D) :- forall(topico(D, T), sabe(T)).`
      * *Tradução:* "O aluno domina a disciplina `D` **SE**, para todo tópico `T` que pertence a `D`, o aluno sabe `T`."

2.  **`findall(Template, Objetivo, ListaResultado)`:**
    Encontra *todas* as soluções possíveis e as guarda numa lista.

    * No código:
        ```prolog
        findall(T, (
				topico(Disciplina, T),
				\+ sabe(T)
        ), Lista).
        ```
    * *Tradução:* "Procure todo `T`, tal que `T` é um tópico da disciplina `Disciplina` **E** o aluno **NÃO** sabe `T`. Coloque todos na variável `Lista`."

3.  **Negação por Falha (`\+`):**
    O operador `\+` significa **NOT**. Em Prolog, algo é falso se não puder ser provado verdadeiro.
	* Em Prolog, "falso" significa "não consegui provar que é verdade"
	* Na linha: `\+ sabe(T)`, o Prolog tenta provar `sabe(T)` buscando por ele no banco de dados. Se falhar **(não encontrar o fato)**, então a negação retorna **Verdadeiro** ("É verdade que eu não sei `T`").

### Módulo 5: Banco de Dados Dinâmico (Memória RAM)

**Onde estudar:** `src/main.pl`

Por padrão, um programa Prolog é estático (você não pode mudar o código enquanto ele roda). Mas o `dynamic` permite isso.

```prolog
:- dynamic sabe/1.
```

Esta diretiva avisa ao Intepretador que fatos `sabe(...)` serão criado e destruídos o tempo todo.
* **`assertz(sabe(calculo))`:** Adiciona esse novo fato na memória.
* **`retract(sabe(calculo))`:** Remove esse fato da memória.
* **`retractall(sabe(_))`:** Apaga todos os fatos `sabe(...)` (conhecimento) da memória (usado ao carregar um novo perfil, para limpar o conhecimento do perfil anterior).

## Módulo 6. Fluxo de Controle e Interface (CLI)

**Onde estudar:** `src/main.pl` e `src/handlers.pl`

Como criar um menu interativo e infinito numa linguagem lógica? Utiliza-se a técnica de **"Repeat-Fail"** com **Backtracking**.

```prolog
menu_loop :-
    repeat,                 % 1. Cria um ponto de retorno.
    mostrar_opcoes,         % 2. Exibe o menu.
    read(Opcao),            % 3. Lê entrada do usuário.
    tratar_opcao(Opcao),    % 4. Executa a lógica.
    Opcao = 0,              % 5. Verifica a condição de saída.
    !                       % 6. Cut (Corte).
.
```
\
**O Backtracking (A mágica do Prolog)**

Se você digitar `1`:

1.  O código executa `tratar_opcao(1)`.
2.  Chega na linha 6: `Opcao = 0`. Como 1 não é igual a 0, isso retorna **FALSO**.
3.  Quando algo dá falso, o Prolog entra em modo de recuperação e volta **(Backtracking)** procurando o ponto de escolha mais próximo (para tentar outra alternativa).
4.  Ele sobe até encontrar o `repeat`. O `repeat` **sempre** diz: "Ainda tenho outra alternativa: tentar de novo".
5.  O código desce novamente, re-imprimindo o menu.

Se você digitar `0`:

1.  Executa `tratar_opcao(0)` (que salva alterações do último perfil carregado).
2.  Chega na linha 6: `0 = 0`. Que é **VERDADEIRO**.
3.  Avança para a linha 7: `!`.
4.  O **Cut (!)** mata o Backtracking. Ele diz: "Se chegou aqui, não volte mais. Encerre com sucesso". O looping é quebrado e o programa termina.

-----

## Módulo 7. Sistema de Persistência de Dados (Personas)

**Onde estudar:** `src/persistence.pl`

A persistência do STICC é baseada em metaprogramação: o Prolog salva os dados escrevendo... código Prolog!

Quando `salvar_no_disco/1` é chamado, ele cria um arquivo de texto e escreve algo como:

```prolog
:- retractall(sabe(_)).  	% Limpa a memória anterior

% ----- 1º Período -----
% Cálculo Diferencial e Integral I:
sabe(limites).           	% Restaura o conhecimento 1
sabe(derivadas).         	% Restaura o conhecimento 2

% Geometria Analítica:
sabe(vetores_operacoes).
sabe(retas_equacoes).

% ----- 5º Período -----
% Inteligência Artificial:
sabe(agentes_inteligentes).
sabe(metodos_busca).

% ----- Nº Período -----
% Disciplina X:
% sabe(...)					% Restaura o conhecimento N
```

Quando o usuário escolhe a opção 5 (`Carregar Persona`), o sistema usa o comando `consult('arquivo.pl')`.
O `consult` simplesmente lê esse arquivo e executa ele como se fosse parte do código original, restaurando o estado da memória instantaneamente.

1.  O comando `retractall` apaga todos os fatos dinâmicos `sabe(...)` que estavam atualmente na RAM.
2.  Os fatos `sabe(...)` da nova persona são carregados na RAM. Restaurando instantaneamente o estado da persona desejada.

-----

## Módulo 8. Guia de Expansão

Para adicionar novas funcionalidades ao projeto, basta seguir o fluxo de dados:

1. **Adicionar Disciplinas:** Apenas edite `src/data.pl` e siga o padrão `disciplina(sigla, 'nome', periodo).`, Não é necessário alterar regras.
2. **Adicionar Conteúdo:** Edite `src/data.pl` seguindo o padrão
	```ProLog
	lista_topicos(sigla, [
		nome_do_tópico_1-'Descrição do tópico 1',
		nome_do_tópico_2-'Descrição do tópico 2',
		nome_do_tópico_N-'Descrição do tópico N',
	]).
	```
3. **Novas Regras:** Edite `src/rules.pl`. Exemplo: Criar uma regra para sugerir disciplinas **optativas** ou **eletivas** baseando-se no perfil do aluno.
4. **Alterar Menus:**
	* Adicione a linha visual (`write(...)`) em `menu_loop/0` (`src/main.pl`).
	* Adicione uma nova cláusula `tratar_opcao(N)` correspondente em `src/handlers.pl`.

OBS: Salvar arquivos usando a codificação `windows1252`.

-----