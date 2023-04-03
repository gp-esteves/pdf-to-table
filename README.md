# PDF-to-table ShinyApp

Olá! Esse é um aplicativo simples baseado em ShinyApp que recebe .PDFs de DEXA (por enquanto apenas do aparelho modelo Horizon) e extrai as tabelas disponíveis no documento, e permite download na forma de arquivo de Excel (.xlsx). Fiz esse aplicativo para aprender um pouco sobre ShinyApp e, se possível, facilitar um pouco a vida dos meus colegas :).

## Como usar

O uso é relativamente simples. Selecione o modelo de DEXA (apenas o Horizon funciona por enquanto!), a região analisada (**importante**, é possível apenas extrair tabelas de uma varredura por vez), o formato da planilha (ver abaixo), e navegar até a localização do arquivo. É possível selecionar apenas um arquivo **ou vários de uma vez**, contanto que sejam todos do mesmo tipo de varredura (por exemplo, 10 pdfs de varreduras de coluna). 

Depois disso é só aguardar alguns instantes (costuma ser rápido!), visualizar a planilha (recomendo uma rápida checagem sempre) e fazer o download no botão disponível.

## Formato original vs. wide

No formato original disponível nos exames, existem linhas diferentes para regiões diferentes da varredura (por exemplo, na coluna, L1, L2, L3, etc), e colunas para as diferentes variáveis da medida (ex., CMO, DMO, etc). Esse formato não é muito próprio para ser analisado na maioria dos programas estatísticos. A opção 'Wide' fornecerá um formato mais adequado, onde cada coluna será uma variável unicamente identificada (ex., L1_CMO, L1_DMO, L2_CMO, L2_DMO). 

## Dúvidas frequentes

**Posso enviar um PDF inteiro com todas as diferentes varreduras (i.e., corpo inteiro, coluna, femur, etc.)?**
Por enquanto, o app não tem a função de receber um PDF com todos as varreduras do DEXA e extrair cada tabela individualmente. Por enquanto, é necessário submeter apenas um tipo de exame por vez. Por isso, é recomendado que você salve cada varredura do seu exame de DXA separadamente (e.g., paciente_A_femur.pdf, paciente_A_coluna.pdf, etc.), e depois submeta aqui (você pode fazer isso com vários pacientes de uma vez, desde seja o mesmo tipo de exame!). 
<br>
**Posso tirar uma foto ou escanear um exame impresso (e.g., Adobe ou Microsoft Lens) e então submeter o PDF aqui? Ou então, PDFs convertidos para conter texto podem funcionar?**
**Não**. O app depende de exames importados diretamente pelos programas de DEXA, pois convertem o texto presente no PDF em strings. A conversão de um exame impresso muitas vezes não faz uma conversão perfeita, e é muito provável que o app não consiga extrair corretamente a tabela. Você pode tentar, mas muito provavelmente não dará certo! Prefira emitir o PDF diretamente pelo software do computador que realiza o DEXA. 
<br>

**Parece que tem algo de errado com a minha tabela! O que eu faço?** 
É muito provavel que surjam alguns erros conforme mais exames diferentes sejam submetidos! Caso você tenha encontrado um erro que eu não encontrei, por favor me envie os detalhes para que eu possa arrumar (se eu conseguir!). Na dúvida, atualize a página (F5) e tente de novo.