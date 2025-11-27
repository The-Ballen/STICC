/*
    ============================================================================
    BASE DE DADOS BASEADA NA MATRIZ CURRICULAR COMPLETA DO CURSO
    ============================================================================
    Estrutura de Disciplinas: disciplina(Sigla, 'Nome', Periodo).
    Estrutura de Tópicos:     lista_topicos(Sigla, [Chave-'Descrição', ...]).
    OBS: Período '9' indica Disciplina Optativa.
    ============================================================================
*/


% ============================================================================
% 1. DISCIPLINAS
% ============================================================================

% --- 1º PERÍODO ---
disciplina(icc,   'Introdução à Ciência da Computação', 1).
disciplina(ip,    'Introdução à Programação', 1).
disciplina(cid1,  'Cálculo Diferencial e Integral I', 1).
disciplina(ga,    'Geometria Analítica', 1).
disciplina(md,    'Matemática Discreta', 1).
disciplina(mc,    'Metodologia Científica', 1).

% --- 2º PERÍODO ---
disciplina(plp,   'Paradigmas de Linguagens de Programação', 2).
disciplina(si,    'Sistemas de Informação', 2).
disciplina(cid2,  'Cálculo Diferencial e Integral II', 2).
disciplina(fcc1,  'Física I para Computação', 2).
disciplina(al,    'Álgebra Linear', 2).
disciplina(lcc,   'Lógica para Ciência da Computação', 2).
disciplina(adm,   'Administração para Ciência da Computação', 2).

% --- 3º PERÍODO ---
disciplina(aed1,  'Algoritmos e Estruturas de Dados I', 3).
disciplina(poo,   'Programação Orientada a Objetos', 3).
disciplina(bd1,   'Banco de Dados I', 3).
disciplina(fcc2,  'Física II para Ciência da Computação', 3).
disciplina(eb,    'Estatística Básica', 3).
disciplina(cd,    'Circuitos Digitais', 3).

% --- 4º PERÍODO ---
disciplina(aed2,  'Algoritmos e Estruturas de Dados II', 4).
disciplina(rc,    'Redes de Computadores', 4).
disciplina(bd2,   'Banco de Dados II', 4).
disciplina(aa,    'Análise de Algoritmos', 4).
disciplina(lfa,   'Linguagens Formais e Autômatos', 4).
disciplina(aoc,   'Arquitetura e Organização de Computadores', 4).
disciplina(epcc,  'Empreendedorismo para Ciência da Computação', 4).

% --- 5º PERÍODO ---
disciplina(ia,    'Inteligência Artificial', 5).
disciplina(comp,  'Construção de Compiladores', 5).
disciplina(cod,   'Computabilidade e Decidibilidade', 5).
disciplina(dni,   'Desenvolvimento de Negócios e Inovação', 5).
disciplina(so,    'Sistemas Operacionais', 5).
disciplina(nt,    'Núcleo Temático', 5).

% --- 6º PERÍODO ---
disciplina(cida,  'Ciência de Dados', 6).
disciplina(es,    'Engenharia de Software', 6).
disciplina(sd,    'Sistemas Distribuídos', 6).

% --- 7º PERÍODO ---
disciplina(tcc1,  'Trabalho de Conclusão de Curso I', 7).
disciplina(psc,   'Projeto de Sistemas Computacionais', 7).
disciplina(ihc,   'Interação Homem-Computador', 7).

% --- 8º PERÍODO ---
disciplina(tcc2,  'Trabalho de Conclusão de Curso II', 8).
disciplina(est,   'Estágio Supervisionado', 8).

% --- OPTATIVAS (CÓDIGO 9) ---
disciplina(tac1,     'Tópicos Avançados I (Data Science & Genéticos)', 9).
disciplina(tac2,     'Tópicos Avançados II (Programação Funcional)', 9).
disciplina(tac3,     'Tópicos Avançados III (PLN)', 9).
disciplina(tac4,     'Tópicos Avançados IV (Algoritmos em Grafos)', 9).
disciplina(tac5,     'Tópicos Avançados V (Web Fullstack)', 9).
disciplina(java_web, 'Java para Desenvolvimento de Aplicações Web', 9).


% ============================================================================
% 2. TÓPICOS DETALHADOS
% ============================================================================

% --- 1º PERÍODO ---

% Baseado em 2025.1
lista_topicos(icc, [
    historia_evolucao_computadores-'Geração Zero a Quinta Geração, válvulas a transistores',
    sistemas_numeracao-'Binário, Octal, Hexadecimal e conversão de bases',
    arquitetura_von_neumann-'CPU, Memória, Barramentos e dispositivos de E/S',
    software_basico-'Conceitos de Sistemas Operacionais e Tradutores',
    etica_informatica-'Impacto social, legislação e ética profissional',
    nocoes_redes_ia-'Visão introdutória sobre conectividade e inteligência'
]).

% Baseado em 2025.1
lista_topicos(ip, [
    algoritmos_basicos-'Lógica de programação, fluxogramas e pseudocódigo',
    tipos_dados_c-'Tipos primitivos: int, float, char, double e modificadores',
    estruturas_controle-'Condicionais (if/else, switch) e Repetição (for, while)',
    vetores_matrizes-'Arrays unidimensionais e multidimensionais, indexação',
    strings-'Manipulação de cadeias de caracteres em C',
    structs-'Registros e definição de tipos compostos (struct, union)',
    funcoes_procedimentos-'Modularização, escopo e passagem de parâmetros',
    ponteiros_alocacao-'Aritmética de ponteiros e alocação dinâmica (malloc/free)',
    arquivos-'Leitura e escrita persistente em arquivos texto e binário'
]).

% Baseado em 2025.1
lista_topicos(cid1, [
    funcoes_reais-'Domínio, Imagem, Composta, Inversa e Gráficos',
    limites_continuidade-'Definição formal, limites laterais e Teorema do Confronto',
    derivadas_regras-'Regra da Cadeia, Produto, Quociente e Derivação Implícita',
    aplicacoes_derivada-'Taxas relacionadas, Máximos, Mínimos e L\'Hopital',
    integrais_indefinidas-'Primitivas imediatas e métodos de substituição',
    teorema_fundamental_calculo-'Integral Definida, Soma de Riemann e cálculo de áreas'
]).

% Baseado em 2025.1
lista_topicos(ga, [
    vetores_operacoes-'Soma, Produto Escalar, Vetorial e Misto no R2 e R3',
    retas_equacoes-'Equações Vetorial, Paramétrica, Simétrica e Reduzida',
    planos_equacoes-'Equação Geral, Vetorial e Paramétrica do plano',
    distancias-'Cálculo de distância Ponto-Reta, Ponto-Plano e Reta-Reta',
    conicas-'Identificação de Elipse, Hipérbole e Parábola'
]).

% Baseado em 2025.1
lista_topicos(md, [
    logica_proposicional-'Conectivos, Tabelas-Verdade e Tautologias',
    logica_predicados-'Quantificadores Universal/Existencial e validade',
    teoria_conjuntos-'Operações, Pertinência, Continência e Produto Cartesiano',
    relacoes_funcoes-'Equivalência, Ordem Parcial, Injetora/Sobrejetora',
    inducao_recursao-'Princípio da Indução Finita e definições recursivas',
    estruturas_algebricas-'Introdução a Monoides, Grupos e Reticulados'
]).

% Baseado em 2025.1
lista_topicos(mc, [
    metodo_cientifico-'Hipótese, Experimentação, Análise e Conclusão',
    tipos_pesquisa-'Qualitativa vs Quantitativa, Exploratória vs Descritiva',
    projeto_pesquisa-'Estrutura: Tema, Justificativa, Objetivos e Cronograma',
    normas_abnt_citacoes-'Regras de formatação, NBR 10520 e NBR 6023',
    etica_plagio-'Integridade acadêmica e direitos autorais',
    artigo_cientifico-'Estrutura, redação técnica e submissão'
]).


% --- 2º PERÍODO ---

% Baseado em 2025.2
lista_topicos(plp, [
    criterios_projeto_linguagens-'Legibilidade, Redigibilidade e Confiabilidade',
    sintaxe_semantica-'BNF, EBNF, Árvores de Derivação e Parsing',
    nomes_vinculacao_escopo-'Vinculação Estática vs Dinâmica e Tempo de Vida',
    tipos_dados_avancados-'Tuplas, Uniões, Ponteiros e Referências',
    expressoes_instrucoes-'Precedência, Associatividade e Curto-circuito',
    subprogramas_implementacao-'Passagem de parâmetros e Registros de Ativação',
    paradigma_funcional-'Imutabilidade e funções puras (LISP/Scheme/Haskell)',
    paradigma_logico-'Fatos, Regras e Inferência (Prolog)'
]).

% Baseado em 2025.2
lista_topicos(si, [
    conceitos_dados_info-'Diferença entre Dado, Informação e Conhecimento',
    tipos_sistemas_info-'SPT, SIG, SAD e SAE',
    etica_privacidade_si-'LGPD, Propriedade Intelectual e Crimes Digitais',
    infraestrutura_ti-'Hardware, Cloud Computing (IaaS, PaaS, SaaS)',
    seguranca_informacao-'Confidencialidade, Integridade e Disponibilidade',
    sistemas_empresariais-'Integração via ERP, CRM e SCM',
    inteligencia_negocios-'Business Intelligence, Data Warehousing e Analytics',
    ciclo_vida_sistemas-'SDLC, Metodologias Ágeis vs Tradicionais'
]).

% Baseado em 2025.2
lista_topicos(cid2, [
    tecnicas_integracao-'Substituição Trigonométrica e Frações Parciais',
    aplicacoes_integral-'Cálculo de Volumes de Revolução e Comprimento de Arco',
    integrais_improprias-'Integrais com limites infinitos ou descontinuidades',
    funcoes_varias_variaveis-'Domínio, Curvas de nível e Limites em Rn',
    derivadas_parciais-'Gradiente, Derivada Direcional e Plano Tangente',
    maximos_minimos_lagrange-'Otimização com Multiplicadores de Lagrange e Hessiana',
    integrais_multiplas-'Integrais Duplas/Triplas e Mudança de Variável (Jacobiano)'
]).

% Baseado em 2025.2
lista_topicos(fcc1, [
    medidas_vetores_fisicos-'Sistema Internacional, Conversão e Operações Vetoriais',
    cinematica_vetorial-'Movimento 2D/3D, Projéteis e Movimento Circular',
    leis_newton_dinamica-'Força, Massa, Atrito e Diagrama de Corpo Livre',
    trabalho_energia-'Energia Cinética/Potencial e Conservação de Energia',
    centro_massa-'Sistemas de Partículas e Colisões',
    momento_linear_colisoes-'Impulso, Colisões Elásticas e Inelásticas'
]).

% Baseado em 2025.2
lista_topicos(al, [
    espacos_vetoriais-'Definição formal, Subespaços, Base e Dimensão',
    transformacoes_lineares-'Núcleo, Imagem e Matriz de Transformação',
    autovalores_autovetores-'Polinômio Característico e Autoespaços',
    diagonalizacao-'Matrizes Diagonalizáveis e mudança de base',
    produto_interno-'Norma, Ortogonalidade e Processo de Gram-Schmidt',
    operadores_especiais-'Operadores Autoadjuntos, Ortogonais e Simétricos'
]).

% Baseado em 2025.2
lista_topicos(lcc, [
    deducao_natural-'Regras de inferência para Lógica Proposicional',
    sistemas_axiomaticos-'Sistemas de Hilbert, Consistência e Completude',
    resolucao_proposicional-'Forma Clausal e Unificação',
    logica_primeira_ordem-'Sintaxe, Semântica e Modelos em LPO',
    clausulas_horn_resolucao-'Resolução SLD (Fundamento do Prolog)',
    programacao_logica_prolog-'Fatos, Regras, Listas e Recursão em Prolog',
    logicas_nao_classicas-'Introdução a Lógicas Modais, Temporais e Multivaloradas'
]).

% Baseado em 2025.2
lista_topicos(adm, [
    teoria_geral_adm-'Escolas Clássica (Taylor/Fayol), Burocrática e Sistêmica',
    estrutura_organizacional-'Organogramas, Departamentalização e Hierarquia',
    gestao_qualidade-'Ferramentas 5S, PDCA, Ishikawa e TQM',
    modelagem_negocios-'Business Model Canvas, Análise SWOT e Estratégia',
    lideranca_motivacao-'Teorias de Motivação e Trabalho em Equipe',
    governanca_ti-'Alinhamento TI-Negócio (Conceitos de COBIT/ITIL)'
]).


% --- 3º PERÍODO ---

% Baseado em 2023.2
lista_topicos(aed1, [
    alocacao_dinamica_avancada-'Gestão manual de memória e vetores dinâmicos',
    tipos_abstratos_dados-'Conceito de TAD e encapsulamento de dados',
    pilhas_filas-'Implementações estáticas (array) e dinâmicas (nós)',
    listas_encadeadas-'Listas Simplesmente, Duplamente e Circularmente encadeadas',
    algoritmos_busca-'Busca Sequencial, Binária e por Interpolação',
    algoritmos_ordenacao_simples-'Bubble Sort, Selection Sort e Insertion Sort',
    algoritmos_ordenacao_eficiente-'Shell Sort, Merge Sort e Quick Sort',
    hashing-'Tabelas de Espalhamento e Tratamento de colisão'
]).

% Baseado em 2023.2
lista_topicos(poo, [
    paradigma_orientado_objetos-'Abstração, troca de mensagens e objetos',
    classes_objetos_java-'Sintaxe Java, JVM, construtores e instanciação',
    encapsulamento_modificadores-'Visibilidade (public, private, protected) e acesso',
    heranca_polimorfismo-'Reuso (extends), sobrescrita e sobrecarga',
    classes_abstratas_interfaces-'Contratos, métodos abstratos e implements',
    tratamento_excecoes-'Blocos try-catch-finally e lançamento de exceções',
    padroes_projeto-'Introdução a padrões como Singleton e Factory'
]).

% Baseado em 2023.2
lista_topicos(bd1, [
    modelagem_conceitual-'Modelo Entidade-Relacionamento (MER/DER)',
    modelo_relacional-'Mapeamento ER para Tabelas e Chaves (PK, FK)',
    algebra_relacional-'Seleção, Projeção, Junção e União',
    sql_ddl_dml-'Comandos SQL: CREATE, ALTER, INSERT, UPDATE, DELETE',
    consultas_avancadas-'JOINs, GROUP BY, HAVING e Subqueries',
    programacao_banco_dados-'Views, Triggers e Stored Procedures básicas',
    normalizacao-'Dependências Funcionais e Formas Normais (1FN a 3FN)'
]).

% Baseado em 2023.2
lista_topicos(fcc2, [
    eletrostatica_coulomb-'Carga Elétrica e Lei de Coulomb',
    lei_gauss_fluxo-'Fluxo Elétrico e Superfície Gaussiana',
    potencial_capacitancia-'Potencial Elétrico, Capacitores e Dielétricos',
    circuitos_eletricos_dc-'Corrente, Resistência, Leis de Kirchhoff e RC',
    campo_magnetico_fontes-'Lei de Biot-Savart e Lei de Ampère',
    inducao_eletromagnetica-'Lei de Faraday, Lei de Lenz e Indutância',
    equacoes_maxwell-'Síntese do Eletromagnetismo (Forma Integral)'
]).

% Baseado em 2023.2
lista_topicos(eb, [
    estatistica_descritiva-'Média, Mediana, Variância e Histogramas',
    probabilidade_teoremas-'Espaço amostral, Teorema de Bayes e Independência',
    distribuicoes_discretas-'Bernoulli, Binomial e Poisson',
    distribuicoes_continuas-'Uniforme, Exponencial e Normal (Gaussiana)',
    amostragem_estimacao-'Teorema Central do Limite e Intervalos de Confiança',
    testes_hipotese-'Testes para médias, proporções e variância'
]).

% Baseado em 2023.2
lista_topicos(cd, [
    algebra_booleana_portas-'Postulados, Teoremas e Portas Lógicas Básicas',
    circuitos_combinacionais-'Codificadores, Multiplexadores e Mapas de Karnaugh',
    circuitos_sequenciais-'Latches e Flip-Flops (SR, JK, D, T)',
    contadores_registradores-'Contadores Síncronos/Assíncronos e Deslocamento',
    maquinas_estados_finitos-'Modelagem de circuitos com Mealy e Moore',
    aritmetica_digital-'Somadores, Subtratores e ULA básica'
]).


% --- 4º PERÍODO ---

% Baseado em 2024.1
lista_topicos(aed2, [
    arvores_binarias-'Definição, percursos (ordem, pré, pós)',
    arvores_busca_binaria-'BST: Inserção, remoção e busca',
    arvores_balanceadas-'Árvores AVL e Vermelho-Preto (Rotações)',
    arvores_b_gerais-'Árvores B para armazenamento secundário',
    heaps_filas_prioridade-'Heap Binário (Min/Max) e HeapSort',
    ordenacao_tempo_linear-'Counting Sort, Radix Sort e Bucket Sort'
]).

% Baseado em 2024.1
lista_topicos(rc, [
    arquitetura_redes_osi_tcp-'Camadas, encapsulamento e modelos de referência',
    camada_fisica_enlace-'Meios de transmissão, Framing e Controle de Acesso (MAC)',
    camada_rede_ip-'Endereçamento IPv4/IPv6, Roteamento e Sub-redes',
    camada_transporte-'Protocolos TCP (Confiabilidade) e UDP',
    camada_aplicacao-'HTTP, DNS, SMTP, DHCP e protocolos de alto nível',
    seguranca_redes-'Firewalls, Criptografia básica e VPNs',
    redes_sem_fio-'Padrões Wi-Fi (802.11) e Mobilidade'
]).

% Baseado em 2024.1
lista_topicos(bd2, [
    processamento_consultas-'Planos de execução e otimização de query',
    transacoes_concorrencia-'Propriedades ACID, Escalonamento e Locks',
    recuperacao_falhas-'Logs, Checkpoints e algoritmos Undo/Redo',
    seguranca_bd-'Controle de acesso, Privilégios e Criptografia no BD',
    bancos_nosql-'Modelos Chave-Valor, Documento, Coluna e Grafo',
    big_data_analytics-'Introdução a processamento de grandes volumes de dados'
]).

% Baseado em 2024.1
lista_topicos(aa, [
    analise_assintotica-'Notações O, Omega e Theta',
    recorrencias-'Teorema Mestre e Árvore de Recursão',
    analise_pior_medio_melhor-'Análise de complexidade de algoritmos clássicos',
    classes_complexidade-'Classes P, NP e NP-Completo (Reduções)',
    logica_hoare_corretude-'Invariantes de laço e provas formais de correção',
    algoritmos_probabilisticos-'Introdução a algoritmos não-determinísticos'
]).

% Baseado em 2024.1
lista_topicos(lfa, [
    linguagens_regulares-'AFD, AFN, Expressões Regulares e Lema do Bombeamento',
    linguagens_livres_contexto-'Gramáticas (GLC), Autômatos de Pilha e Chomsky',
    maquinas_turing-'Definição, Variações e Tese de Church-Turing',
    decidibilidade-'Problema da Parada e Conjuntos Enumeráveis',
    hierarquia_chomsky-'Classificação e relacionamento das linguagens formais'
]).

% Baseado em 2024.1
lista_topicos(aoc, [
    estrutura_funcao_computador-'Barramentos, Ciclo de Instrução e Interconexão',
    conjunto_instrucoes_assembly-'Arquitetura MIPS/RISC e Modos de endereçamento',
    pipeline_processamento-'Estágios de execução e tratamento de Hazards',
    hierarquia_memoria-'Cache (Mapeamento Direto/Associativo) e Memória Virtual',
    entrada_saida-'Interrupções, DMA e Interfaces de E/S',
    arquiteturas_paralelas-'Multiprocessadores e arquiteturas Superescalares'
]).

% Baseado em 2024.1
lista_topicos(epcc, [
    perfil_empreendedor-'Características comportamentais e Visão de futuro',
    plano_negocios-'Estrutura do plano e Sumário Executivo',
    modelagem_negocios_canvas-'Business Model Canvas e Proposta de Valor',
    matriz_swot-'Análise de Forças, Fraquezas, Oportunidades e Ameaças',
    plano_marketing_financeiro-'Estratégias de venda, Custos e Fluxo de caixa',
    ecosistema_inovacao-'Startups, Investidores Anjo e Incubadoras'
]).


% --- 5º PERÍODO ---

% Baseado em 2023.2
lista_topicos(ia, [
    agentes_inteligentes-'Agentes Reativos, Baseados em Objetivos e Aprendizagem',
    metodos_busca-'Busca Cega (Largura/Profundidade) e Heurística (A*, Gulosa)',
    representacao_conhecimento-'Lógica, Regras de Produção e Redes Semânticas',
    sistemas_especialistas-'Motor de Inferência e Base de Conhecimento',
    logica_fuzzy-'Conjuntos difusos e Graus de Pertinência',
    aprendizado_maquina_basico-'Introdução a Redes Neurais e Classificadores',
    algoritmos_otimizacao-'Algoritmos Genéticos e Têmpera Simulada'
]).

% Baseado em 2023.2
lista_topicos(comp, [
    estrutura_compilador-'Fases: Front-end vs Back-end',
    analise_lexica-'Identificação de Tokens e Expressões Regulares (Lex)',
    analise_sintatica-'Gramáticas, Parsing Ascendente/Descendente (Yacc)',
    tabela_simbolos-'Gerenciamento de escopo e amarração de tipos',
    analise_semantica-'Verificação de tipos e Árvores decoradas',
    geracao_codigo_intermediario-'Código de três endereços e Máquinas Virtuais',
    otimizacao_codigo-'Otimização em Blocos básicos e Grafos de fluxo'
]).

% Baseado em 2023.2
lista_topicos(cod, [
    logica_primeira_ordem_formal-'Sintaxe e Semântica formal, Cálculo de Hilbert',
    funcoes_recursivas-'Funções Primitivas, Minimização e Teorema s-m-n',
    maquinas_registros_ilimitados-'URM (Unlimited Register Machine) e computabilidade',
    tese_church_turing-'Equivalência entre modelos computacionais',
    decidibilidade_indecidibilidade-'Problema da Parada e Teorema de Rice',
    teoremas_incompletude-'Teoremas de Gödel e Aritmética de Presburg',
    programa_universal-'Enumeração de Gödel e Universalidade'
]).

% Baseado em 2025.1
lista_topicos(dni, [
    inovacao_tipologias-'Inovação Disruptiva, Social, Aberta e Incremental',
    tecnologias_disruptivas-'Impacto econômico e social de novas tecnologias',
    modelagem_novos_negocios-'Lean Startup e MVP (Produto Mínimo Viável)',
    metodos_ageis-'Scrum, Kanban e Design Thinking para inovação',
    marketing_estrategico-'Marketing Digital e Branding para novos negócios',
    lideranca_disruptiva-'Gestão de equipes ágeis e Líder Coach',
    sistema_nacional_inovacao-'Fomento, Marco Legal e Propriedade Intelectual'
]).

% Baseado em 2023.2
lista_topicos(so, [
    gerencia_processos-'Processos vs Threads, Estados e Contexto',
    escalonamento_processos-'Algoritmos FIFO, SJF, Round Robin e Prioridade',
    concorrencia_sincronizacao-'Semáforos, Monitores e Impasses (Deadlocks)',
    gerencia_memoria-'Paginação, Segmentação e Memória Virtual',
    sistemas_arquivos-'Estrutura de diretórios, i-nodes e FAT/NTFS',
    entrada_saida-'Drivers, DMA, Spooling e escalonamento de Disco',
    programacao_concorrente-'Prática com Threads e Mutex'
]).

% Baseado em 2024.1
lista_topicos(nt, [
    computacao_desplugada-'Ensino de algoritmos sem uso de computador',
    jogos_educativos-'Gamificação aplicada ao ensino básico',
    metodologias_ativas-'Sala de aula invertida e Aprendizagem baseada em projetos',
    iot_educacao-'Introdução à Robótica educacional',
    mineracao_dados_educacao-'Análise de desempenho escolar com dados',
    desenvolvimento_projetos_ext-'Aplicação prática em escolas reais',
    computacao_blocos-'Programação visual com Scratch/Blockly'
]).


% --- 6º PERÍODO ---

% Baseado em 2024.1
lista_topicos(cida, [
    fundamentos_ml-'Aprendizado Supervisionado, Não-supervisionado e por Reforço',
    pre_processamento_dados-'Limpeza, Normalização e Feature Engineering',
    algoritmos_classificacao-'Naive Bayes, KNN e SVM',
    redes_neurais_artificiais-'Perceptron, MLP, Backpropagation e Funções de Ativação',
    deep_learning-'CNN (Convolucional), RNN, LSTM e Autoencoders',
    transformers_llm-'Arquitetura BERT, GPT e Mecanismo de Atenção',
    etica_ia-'Viés algorítmico, Justiça e Explicabilidade'
]).

% Baseado em 2024.1
lista_topicos(es, [
    processos_software-'Ciclo de vida: Cascata, Espiral e Ágil',
    engenharia_requisitos-'Elicitação, Análise, Documentação e UML',
    arquitetura_software-'Padrões arquiteturais e introdução a Design Patterns',
    testes_software-'Testes Unitários, Integração, Sistema e Aceitação',
    gestao_configuracao-'Controle de versão e CI/CD (Integração Contínua)',
    manutencao_software-'Manutenção Corretiva, Evolutiva e Refatoração'
]).

% Baseado em 2024.1
lista_topicos(sd, [
    comunicacao_distribuida-'RPC, RMI, Sockets e Mensageria',
    sincronizacao_relogios-'Algoritmos de Berkley, Cristian e Exclusão Mútua',
    algoritmos_distribuidos-'Algoritmos de Eleição e Detecção de Deadlock',
    transacoes_distribuidas-'Commit em duas fases (2PC) e Concorrência',
    replicacao_consistencia-'Modelos de consistência e Teorema CAP',
    computacao_nuvem-'Conceitos de IaaS, PaaS, SaaS e Virtualização'
]).


% --- 7º PERÍODO ---

% Baseado em 2022.2
lista_topicos(tcc1, [
    delimitacao_tema-'Definição e escolha do problema de pesquisa',
    revisao_sistematica-'Busca em bases científicas e Estado da arte',
    escrita_cientifica-'Normas ABNT e Estrutura de monografia',
    metodologia_pesquisa-'Classificação, Hipóteses, Materiais e Métodos',
    projeto_pesquisa-'Elaboração, escrita e defesa do pré-projeto'
]).

% Baseado em 2023.2
lista_topicos(psc, [
    analise_projeto_oo-'Reforço de POO aplicado em nível de sistema',
    modelagem_uml_avancada-'Diagramas de Sequência, Atividades e Estados',
    metodologias_ageis-'Prática de Scrum, Kanban e User Stories',
    devops_ci_cd-'Pipelines de deploy e Containerização (Docker)',
    gestao_projetos-'Estimativas, Análise de Riscos e Monitoramento',
    mvp-'Desenvolvimento e entrega de Produto Mínimo Viável'
]).

% Baseado em 2023.2
lista_topicos(ihc, [
    design_centrado_usuario-'Criação de Personas e Cenários de uso',
    psicologia_cognitiva-'Percepção, Memória, Atenção e Modelos Mentais',
    prototipacao_interfaces-'Wireframes, Mockups e Ferramentas (Figma)',
    acessibilidade_design_universal-'Normas WCAG e Tecnologias assistivas',
    avaliacao_usabilidade-'Heurísticas de Nielsen e Testes com usuários',
    design_responsivo-'Adaptação de interfaces para dispositivos móveis'
]).


% --- 8º PERÍODO ---

% Baseado em 2023.1
lista_topicos(tcc2, [
    desenvolvimento_tcc-'Implementação da solução ou Estudo de Caso',
    escrita_monografia-'Análise de Resultados, Discussão e Conclusão',
    defesa_publica-'Apresentação final para banca examinadora'
]).

% (Estágio mantém estimado pois é atividade prática externa variável)
lista_topicos(est,  [
    vivencia_profissional-'Atuação prática em ambiente corporativo',
    relatorios_tecnicos-'Documentação das atividades desenvolvidas no estágio'
]).


% --- OPTATIVAS ---

% Baseado em 2022.2 (Tópicos I)
lista_topicos(tac1, [
    limpeza_tratamento_dados-'Tratamento de dados faltantes e outliers',
    visualizacao_dados-'Geração de Histogramas e Scatter plots',
    classificacao_supervisionada-'Árvores de Decisão e KNN',
    metricas_avaliacao_ml-'Acurácia, Precisão, Recall e F1-Score',
    algoritmos_geneticos-'Seleção, Cruzamento, Mutação e Elitismo',
    problemas_otimizacao-'Maximização/Minimização com restrições'
]).

% Baseado em 2023.2 (Tópicos II)
lista_topicos(tac2, [
    lambda_calculo-'Fundamentos matemáticos da computação funcional',
    haskell_intro-'Sintaxe básica, Tipos, Listas e Tuplas',
    funcoes_alta_ordem-'Uso de Map, Filter e Fold',
    tipos_algebricos-'ADTs e Pattern Matching',
    polimorfismo-'Polimorfismo Paramétrico e Ad-hoc (Typeclasses)',
    functors_monads-'Tratamento de efeitos colaterais controlados',
    paralelismo_funcional-'Estratégias de avaliação paralela'
]).

% Baseado em 2023.2 (Tópicos III)
lista_topicos(tac3, [
    tokenizacao_pos_tagging-'Processamento básico de texto e etiquetagem',
    analise_sentimento-'Classificação de opinião e polaridade',
    modelagem_topicos-'Algoritmos como LDA (Latent Dirichlet Allocation)',
    redes_neurais_rnn_lstm-'Processamento sequencial de texto',
    transformers_bert_gpt-'Modelos de linguagem modernos baseados em atenção',
    chatbots_assistentes-'Criação de sistemas de diálogo'
]).

% Baseado em 2023.1 (Tópicos IV)
lista_topicos(tac4, [
    grafos_digrafos-'Representação via Matriz e Lista de Adjacência',
    buscas_largura_profundidade-'Algoritmos BFS e DFS',
    ordenacao_topologica-'Resolução de dependências em grafos',
    arvores_geradoras_minimas-'Algoritmos de Kruskal e Prim',
    caminhos_minimos_dijkstra-'Dijkstra, Bellman-Ford e Floyd-Warshall',
    fluxo_maximo-'Algoritmo de Ford-Fulkerson'
]).

% Baseado em 2023.2 (Tópicos V)
lista_topicos(tac5, [
    arquitetura_web-'Modelo Cliente-Servidor e Protocolo HTTP',
    frontend_html_css_js-'Frameworks como Bootstrap e Tailwind',
    backend_php_laravel-'Desenvolvimento back-end com MVC',
    react_js-'Componentes, Estado e Props no Front-end',
    api_rest-'Intercâmbio de dados JSON e Verbos HTTP',
    git_github_trello-'Versionamento de código e Gestão Ágil'
]).

% Baseado em 2023.1 (Java Web)
lista_topicos(java_web, [
    plataforma_java_ee-'Ecossistema Enterprise e Servidores de Aplicação',
    servlets_jsp-'Tecnologias base para web em Java',
    mvc_mvp-'Padrões de arquitetura para web',
    acesso_banco_jdbc-'Conexão e manipulação de SGBD com Java',
    multithreading_concorrencia-'Uso de Threads em aplicações Java',
    frameworks_web_java-'Visão geral de JSF e Spring'
]).


% ============================================================================
% 3. PRÉ-REQUISITOS
% ============================================================================

% --- 2º PERÍODO ---
requer(plp, ponteiros_alocacao).
requer(plp, funcoes_procedimentos).
requer(plp, structs).
requer(si, historia_evolucao_computadores).
requer(si, metodo_cientifico).
requer(cid2, integrais_indefinidas).
requer(cid2, teorema_fundamental_calculo).
requer(cid2, derivadas_regras).
requer(fcc1, derivadas_regras).
requer(fcc1, vetores_operacoes).
requer(al, vetores_operacoes).
requer(al, retas_equacoes).
requer(lcc, logica_proposicional).
requer(lcc, logica_predicados).
requer(lcc, inducao_recursao).
requer(adm, metodo_cientifico).

% --- 3º PERÍODO ---
requer(aed1, ponteiros_alocacao).
requer(aed1, structs).
requer(poo, algoritmos_basicos).
requer(poo, structs).
requer(bd1, teoria_conjuntos).
requer(bd1, conceitos_dados_info).
requer(fcc2, integrais_multiplas).
requer(fcc2, vetores_operacoes).
requer(fcc2, trabalho_energia).
requer(eb, integrais_improprias).
requer(eb, teoria_conjuntos).
requer(cd, sistemas_numeracao).
requer(cd, logica_proposicional).

% --- 4º PERÍODO ---
requer(aed2, alocacao_dinamica_avancada). % De AED-1
requer(aed2, algoritmos_busca).           % De AED-1
requer(rc, arquitetura_von_neumann).      % De ICC
requer(rc, sistemas_numeracao).           % De ICC
requer(bd2, sql_ddl_dml).                 % De BD-1
requer(bd2, normalizacao).                % De BD-1
requer(aa, inducao_recursao).             % De MD
requer(aa, algoritmos_ordenacao_eficiente).% De AED-1
requer(aa, logica_predicados).            % De LCC
requer(lfa, teoria_conjuntos).            % De MD
requer(lfa, deducao_natural).             % De LCC
requer(aoc, circuitos_sequenciais).       % De CD
requer(aoc, maquinas_estados_finitos).    % De CD
requer(aoc, ponteiros_alocacao).          % De IP
requer(epcc, modelagem_negocios).         % De ADM

% --- 5º PERÍODO ---
requer(ia, logica_primeira_ordem).
requer(ia, paradigma_logico).
requer(comp, sintaxe_semantica).
requer(comp, automatos_finitos).       % De LFA
requer(cod, maquinas_turing).          % De LFA
requer(dni, plano_negocios).           % De EPCC
requer(so, interrupcoes).              % De AOC
requer(so, estrutura_funcao_computador).% De AOC
requer(so, pilhas_filas).               % De AED-1
requer(nt, metodo_cientifico).

% --- 6º PERÍODO ---
requer(cida, inteligencia_negocios).
requer(cida, estatistica_descritiva).
requer(cida, testes_hipotese).
requer(sd, camada_transporte).         % De RC
requer(sd, threads).                   % De SO

% --- 7º PERÍODO ---
requer(psc, processos_software).
requer(psc, engenharia_requisitos).
requer(psc, sql_ddl_dml).
requer(psc, paradigma_orientado_objetos).
requer(ihc, engenharia_requisitos).
requer(ihc, classes_objetos_java).
requer(tcc1, metodo_cientifico).
requer(tcc1, normas_abnt_citacoes).

% --- 8º PERÍODO ---
requer(tcc2, projeto_pesquisa).
requer(tcc2, escrita_cientifica).

% --- OPTATIVAS (REQUISITOS LÓGICOS) ---

% TAC I (Data Science): Requer estatística e programação
requer(tac1, estatistica_descritiva).   % De EB
requer(tac1, algoritmos_basicos).       % De IP

% TAC II (Funcional): Requer paradigmas
requer(tac2, paradigma_funcional).      % De PLP
requer(tac2, inducao_recursao).         % De MD

% TAC III (NLP): Requer IA e probabilidade
requer(tac3, aprendizado_maquina_basico). % De IA
requer(tac3, probabilidade_teoremas).   % De EB

% TAC IV (Grafos): Requer estruturas de dados avançadas e análise
requer(tac4, grafos).                   % De AED-2
requer(tac4, analise_assintotica).      % De AA

% TAC V (Web Fullstack): Requer POO e BD
requer(tac5, paradigma_orientado_objetos). % De POO
requer(tac5, sql_ddl_dml).              % De BD-1

% Java Web: Requer POO sólida e BD
requer(java_web, paradigma_orientado_objetos). % De POO
requer(java_web, sql_ddl_dml).          % De BD-1