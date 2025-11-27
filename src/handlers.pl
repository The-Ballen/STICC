/*
    • tratar_opcao(X)
    Cláusulas que definem o comportamento para cada entrada numérica.
 */
% [0] SAIR
tratar_opcao(0) :- 
    salvar_progresso_se_ativo,
    write('Bons estudos!'),
    clear(3),
    !
.

% [1] LISTAR DISCIPLINAS
tratar_opcao(1) :-
    write('-------- GRADE CURRICULAR --------'), nl,

    % Itera pelos períodos de 1 a 9 (9 = optativas)
    forall(between(1, 9, P), (
            % Busca todas as disciplinas daquele período P
            findall(S-N, disciplina(S, N, P), Discs), (
                Discs \= [] -> (            % Verifica se a lista é vazia
                        (
                            P = 9 ->        % Imprime "Optativas" se P=9, ou "Xº Período" se p!=9
                                    Format = '------ Optativas ------~n', Args = []
                                ;
                                    Format = '----- ~wº Periodo -----~n', Args = [P]
                        ),
                        format(Format, Args),
                        imprimir_lista_simples(Discs)
                    )
                    ;
                    true  % Se a lista era vazia, não faz nada (true)
            )
        )
    ),

    !,
    nl
.

% [2] MEU PERFIL
tratar_opcao(2) :-
    write('-------- MEU PERFIL --------'),
    nl,
    % Encontra todas as disciplinas onde o usuário sabe pelo menos UM tópico.
    findall(D, (disciplina(D, _, _), topico(D, T), sabe(T)), DiscsRaw),
    sort(DiscsRaw, DiscsUnicas), % Remove duplicatas.
    
    (DiscsUnicas == [] ->
            write('Perfil vazio. Vá estudar!')
        ;
            % Para cada disciplina encontrada...
            forall(member(D, DiscsUnicas), (
                disciplina(D, Nome, _),
                % Verifica se domina totalmente ou parcialmente.
                (domina(D) ->
                        format('~w (~w):~n   [CONCLUÍDA] Você domina todos os tópicos!~n', [Nome, D])
                    ;
                        findall(T, (topico(D, T), sabe(T)), Sabidos),
                        format('~w (~w):~n   Parcial: ~w~n', [Nome, D, Sabidos])
                )
            ))
    ),

    !,
    nl
.

% [3] APRENDER (Com opção 'todos')
tratar_opcao(3) :-
    prompt1('Sigla da disciplina: '),
    read(D),

    (disciplina(D, Nome, _) ->
            % Pega a lista completa (ID-Descrição) para mostrar ao usuário.
            lista_topicos(D, ListaCompleta),
            
            format('===== Conteúdo de ~w =====~n', [Nome]),
            imprimir_topicos_com_descricao(ListaCompleta),
            
            nl, 
            prompt1('Digite o ID do tópico (ex: limites) ou \'todos\': '),
            read(Input),
            
            (Input == todos ->
                    % Itera sobre a lista e dá assertz em tudo que falta.
                    forall(member(T-_, ListaCompleta), (
                        \+ sabe(T) -> assertz(sabe(T)) ; true
                    )),
                    format('~nTodos os tópicos de ~w foram marcados como aprendidos!~nParabéns!~n', [Nome])
                ;
                    topico(D, Input) ->     % Valida se o input é um tópico real daquela disciplina antes de salvar.
                            assertz(sabe(Input)),
                            write('Registrado!')
                        ;   
                            write('Erro: Tópico inválido.')
            )
        ;
            write('Disciplina não encontrada.')
    ),

    !,
    nl
.

% [4] VERIFICAR PRÉ-REQUISITOS
tratar_opcao(4) :-
    prompt1('Sigla da disciplina: '),
    read(D),

    (disciplina(D, Nome, _) ->
            verificar_defasagem(D, Defasagem),
            (Defasagem == [] ->
                    format('>> [OK] Base sólida para cursar ~w.~n', [Nome])
                ;
                    format('>> Defasagem encontrada para ~w!~n', [Nome]),
                    write('Tente dominar estes conceitos antes:'),
                    nl,
                    listar_pendencias_detalhado(Defasagem)
            )
        ;
            write('Erro: Sigla inválida!')
    ),

    !,
    nl
.

% [5] CARREGAR PERSONA
tratar_opcao(5) :-
    write('----- PERSONAS DISPONÍVEIS -----'),
    nl,

    listar_arquivos_personas(Arquivos),

    (Arquivos == [] ->
            write('(Nenhuma persona salva encontrada)'),
            nl
        ;
            imprimir_arquivos_indexados(Arquivos, 1),
            nl,
            prompt1('Persona: '),
            read(Index),
            
            % Verifica se o número digitado é válido para a lista.
            (integer(Index), nth1(Index, Arquivos, NomeArq) ->
                    % Concatena o caminho da pasta personas e tenta carregar
                    atom_concat('personas/', NomeArq, Path),

                    (exists_file(Path) ->
                            consult(Path),
                            definir_arquivo_atual(Path),
                            format('\'~w\' Carregado com sucesso!~n', [NomeArq])
                        ;
                            write('Erro: Arquivo não encontrado ou ilegível.')
                    )
                ;
                    write('Número inválido.')
            )
    ),

    !,
    nl
.

% [6] GUIA DE ESTUDOS
tratar_opcao(6) :-
    prompt1('Para qual disciplina você deseja estudar? (Sigla): '),
    read(D),

    (disciplina(D, Nome, _) ->
            % Passo 1: Verifica o passado (Requisitos)
            verificar_defasagem(D, Defasagem),
            (Defasagem \= [] ->
                    format('~AVISO: DEFASAGEM DETECTADA~n'),
                    format('Você não domina a base (pré-requisitos) para essa disciplina:~n'),

                    listar_pendencias_detalhado(Defasagem),
                    nl
                ;
                    true
            ),

            % Passo 2: Verifica o presente (Conteúdo da matéria)
            format('~n===== CONTEÚDOS PENDENTES: ~w =====~n', [Nome]),
            topicos_faltantes(D, Conteudos),
            (Conteudos == [] ->
                write('Não há conteúdos pendentes, você já domina essa disciplina.')
            ;
                listar_itens_com_descricao(D, Conteudos)
            )
        ;
            write('Disciplina não encontrada!')
    ),

    !,
    nl
.

% [7] CRIAR PERSONA
tratar_opcao(7) :-
    prompt1('Nome da nova persona (minúsculo): '),
    read(Nome),

    (atom(Nome) ->
            % Cria pasta de personas se não existir
            (exists_directory('personas') ->
                    true
                ;
                    make_directory('personas')
            ),
            
            % Monta caminho: personas/nome.pl
            atom_concat('personas/', Nome, Temp),
            atom_concat(Temp, '.pl', Caminho),
            
            (exists_file(Caminho) ->
                    format('Já existe uma persona com esse nome!~n')
                ;
                    % Cria arquivo novo com template básico
                    open(Caminho, write, Stream),
                    format(Stream, '% PERFIL: ~w~n~n:- retractall(sabe(_)).~n~n', [Nome]),
                    close(Stream),
                    % Carrega a persona criada
                    consult(Caminho),
                    definir_arquivo_atual(Caminho),
                    format('Persona \'~w\' criada e carregada com sucesso!~n', [Nome])
            )
        ;
            write('Nome inválido!')
    ),

    !,
    nl
.

/*
    Tratamento para opção inválida do menu
 */
tratar_opcao(_) :-
    format('Opção inválida!~n')
.