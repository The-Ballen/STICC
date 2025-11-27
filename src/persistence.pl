% ============================================================================
% SISTEMA DE PERSISTÊNCIA (PERSONAS)
% ============================================================================

/*
    Gerencia o estado da sessão. Como Prolog não tem variáveis globais tradicionais,
    o fato dinâmico 'arquivo_atual' é usado para "lembrar" qual persona está carregada.
 */
definir_arquivo_atual(Caminho) :-
    retractall(arquivo_atual(_)),       % 1. Apaga qualquer registro anterior.
    assertz(arquivo_atual(Caminho))     % 2. Cria o novo registro.
.

/*
    Caso exista alguma persona carregada, tenta salvar o progresso ao sair.
 */
salvar_progresso_se_ativo :-
    arquivo_atual(Caminho),     % Verifica se o fato existe.
    !,                          % CUT (!): Se chegou aqui, pare. Não tente a regra de baixo.
    salvar_no_disco(Caminho)    % Chama a função real de salvar.
.
/*
    Regra 2 (Fallback): Executa se a regra acima falhar (quando não há uma persona carregada)
 */
salvar_progresso_se_ativo :-
    write('Nenhum perfil foi carregado. As alterações serão perdidas.'), nl
.

/*
    Salva alterações da persona no disco
 */
salvar_no_disco(Caminho) :-
    format('Salvando alterações em "~w"...~n', [Caminho]),
    
    % 'open' abre um Stream (fluxo de dados) para escrita ('write')
    open(Caminho, write, Stream),
    
    % Escreve o cabeçalho padrão do arquivo
    format(Stream, '% ==========================================~n', []),
    format(Stream, '% PERFIL ATUALIZADO PELO STICC~n', []),
    format(Stream, '% ==========================================~n', []),
    
    % Escreve a diretiva que limpa a memória ao carregar esta persona futuramente
    format(Stream, ':- retractall(sabe(_)).~n~n', []),
    
    % Loop por períodos (1 a 9)
    forall(between(1, 9, P), (
        % Encontra disciplinas deste período que tenham pelo menos UM tópico aprendido (sabe)
        findall(Disc, (disciplina(Disc, _, P), topico(Disc, T), sabe(T)), DiscsRaw),
        sort(DiscsRaw, DiscsUnicas), % Remove duplicatas da lista de disciplinas
        
        % Se houver disciplinas com conteúdo nesse período, escreve o cabeçalho do período
        (DiscsUnicas \= [] ->
                (P = 9 -> Label = 'Optativas' ; format(atom(Label), '~wº Período', [P])),
                format(Stream, '% ----- ~w -----~n', [Label]),
                
                % Para cada disciplina encontrada, escreve o nome e os tópicos
                forall(member(D, DiscsUnicas), (
                    disciplina(D, Nome, _),
                    format(Stream, '% ~w:~n', [Nome]),
                    
                    % Escreve apenas os tópicos que pertencem a esta disciplina E que o aluno sabe
                    forall((topico(D, Topico), sabe(Topico)), (
                        format(Stream, 'sabe(~w).~n', [Topico])
                    )),
                    nl(Stream) % Linha em branco após cada disciplina
                )),
                nl(Stream) % Linha em branco após cada período
            ;
                true % Se não souber nada desse período, não escreve nada
        )
    )),
    
    % Fecha o stream aberto (liberando o arquivo)
    close(Stream),
    write('Concluído!'),
    nl
.

/*
    Lista arquivos '.pl'.
    Procura inicialmente na pasta 'personas'.
    caso não encontre, retorna uma lista vazia.
 */
listar_arquivos_personas(A) :-
    (exists_directory('personas') -> 
        directory_files('personas', T), include(is_pl, T, A)
    ; 
        A = [] % Se a pasta não existir, retorna lista vazia e não busca na raiz
    )
.

/*
    Filtra apenas arquivos que terminam com .pl
 */
is_pl(F) :- sub_atom(F, _, 3, 0, '.pl').