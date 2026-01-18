library(SPARQL)
library(DT)

endpoint <- "http://dayhoff.inf.um.es:3049/blazegraph/namespace/ALZgrafo/sparql"

query0 <- "
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX alz_o: <http://alzheimer_ontologia.um.es/>

SELECT ?sujeto ?predicado ?objeto
WHERE {
  ?sujeto ?predicado ?objeto .
  FILTER(STRSTARTS(STR(?sujeto), \"http://dayhoff.inf.um.es:8189/data/\"))
}
LIMIT 50
"

resultado0 <- SPARQL(endpoint, query0)

limpiar_literal <- function(x) {
  if (is.character(x)) {
    x <- gsub('^\"([^\"]*)\"(@[a-z]+)?$', '\\1', x)
    x <- gsub('^http://dayhoff.inf.um.es:8189/data/(.+)$', '\\1', x)
  }
  return(x)
}

for(col in colnames(resultado0$results)) {
  resultado0$results[[col]] <- sapply(resultado0$results[[col]], limpiar_literal)
}

datatable(resultado0$results, 
          caption = "Consulta Test: Primeras 50 tripletas del grafo de Alzheimer",
          options = list(pageLength = 10))

query1 <- "
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX alz_o: <http://alzheimer_ontologia.um.es/>

SELECT ?variante ?nombreVariante ?gen ?nombreGen ?frecuencia
WHERE {
  ?variante alz_o:afectaGen ?gen .
  ?variante rdfs:label ?varianteLabel .
  ?gen rdfs:label ?genLabel .
  ?variante alz_o:tieneFrecuenciaAlelica ?frecuencia .
  BIND(STR(?varianteLabel) AS ?nombreVariante)
  BIND(STR(?genLabel) AS ?nombreGen)
  FILTER(STRSTARTS(STR(?variante), \"http://dayhoff.inf.um.es:8189/data/\"))
}
ORDER BY DESC(?frecuencia)
"

resultado1 <- SPARQL(endpoint, query1)

datatable(resultado1$results, 
          caption = "Consulta 1: Variantes genéticas asociadas a Alzheimer",
          options = list(pageLength = 10))

query2 <- "
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX alz_o: <http://alzheimer_ontologia.um.es/>
PREFIX so: <http://purl.obolibrary.org/obo/SO_>

SELECT DISTINCT ?nombreGen ?nombreProteina ?nombreProceso
WHERE {
  ?gen a so:0000704 .
  ?gen rdfs:label ?genLabel .
  ?gen alz_o:codifica ?proteina .
  ?proteina rdfs:label ?proteinaLabel .
  ?proteina alz_o:participaEn ?proceso .
  ?proceso rdfs:label ?procesoLabel .
  BIND(STR(?genLabel) AS ?nombreGen)
  BIND(STR(?proteinaLabel) AS ?nombreProteina)
  BIND(STR(?procesoLabel) AS ?nombreProceso)
  FILTER(STRSTARTS(STR(?gen), \"http://dayhoff.inf.um.es:8189/data/\"))
}
ORDER BY ?nombreGen
"

resultado2 <- SPARQL(endpoint, query2)

datatable(resultado2$results, 
          caption = "Consulta 2: Cascada gen-proteína-función",
          options = list(pageLength = 10))

query3 <- "
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX alz_o: <http://alzheimer_ontologia.um.es/>

SELECT ?variante ?nombreVariante ?gen ?nombreGen ?efecto
WHERE {
  {
    ?variante alz_o:aumentaRiesgo <http://dayhoff.inf.um.es:8189/data/AlzheimerDisease> .
    ?variante rdfs:label ?varianteLabel .
    ?variante alz_o:afectaGen ?gen .
    ?gen rdfs:label ?genLabel .
    BIND(STR(?varianteLabel) AS ?nombreVariante)
    BIND(STR(?genLabel) AS ?nombreGen)
    BIND(\"Aumenta riesgo\" AS ?efecto)
  }
  UNION
  {
    ?variante alz_o:protegeDe <http://dayhoff.inf.um.es:8189/data/AlzheimerDisease> .
    ?variante rdfs:label ?varianteLabel .
    ?variante alz_o:afectaGen ?gen .
    ?gen rdfs:label ?genLabel .
    BIND(STR(?varianteLabel) AS ?nombreVariante)
    BIND(STR(?genLabel) AS ?nombreGen)
    BIND(\"Protector\" AS ?efecto)
  }
  UNION
  {
    ?variante alz_o:causaMutacion <http://dayhoff.inf.um.es:8189/data/AlzheimerDisease> .
    ?variante rdfs:label ?varianteLabel .
    ?variante alz_o:afectaGen ?gen .
    ?gen rdfs:label ?genLabel .
    BIND(STR(?varianteLabel) AS ?nombreVariante)
    BIND(STR(?genLabel) AS ?nombreGen)
    BIND(\"Causal (familiar)\" AS ?efecto)
  }
}
ORDER BY ?efecto ?nombreVariante
"

resultado3 <- SPARQL(endpoint, query3)

datatable(resultado3$results, 
          caption = "Consulta 3: Variantes por efecto clínico",
          options = list(pageLength = 10))

query4 <- "
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX alz_o: <http://alzheimer_ontologia.um.es/>

SELECT DISTINCT ?categoria ?fenotipoNombre ?comentario
WHERE {
  <http://dayhoff.inf.um.es:8189/data/AlzheimerDisease> alz_o:tieneFenotipo ?categoriaFenotipo .
  ?categoriaFenotipo rdfs:label ?categoriaLabel .
  ?categoriaFenotipo alz_o:tieneFenotipo ?fenotipo .
  ?fenotipo rdfs:label ?fenotipoLabel .
  BIND(STR(?categoriaLabel) AS ?categoria)
  BIND(STR(?fenotipoLabel) AS ?fenotipoNombre)
  
  OPTIONAL { 
    ?fenotipo rdfs:comment ?comentarioRaw .
    BIND(STR(?comentarioRaw) AS ?comentario)
  }
}
ORDER BY ?categoria ?fenotipoNombre
"

resultado4 <- SPARQL(endpoint, query4)

datatable(resultado4$results, 
          caption = "Consulta 4: Manifestaciones clínicas organizadas",
          options = list(pageLength = 15))

query5 <- "
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX alz_o: <http://alzheimer_ontologia.um.es/>

SELECT ?farmaco ?nombreFarmaco ?comentario ?mecanismo
WHERE {
  <http://dayhoff.inf.um.es:8189/data/AlzheimerDisease> alz_o:tratadaCon ?farmaco .
  ?farmaco rdfs:label ?farmacoLabel .
  ?farmaco rdfs:comment ?comentarioRaw .
  BIND(STR(?farmacoLabel) AS ?nombreFarmaco)
  BIND(STR(?comentarioRaw) AS ?comentario)
  
  OPTIONAL {
    ?farmaco alz_o:participaEn ?mecanismoAccion .
    ?mecanismoAccion rdfs:label ?mecanismoLabel .
    BIND(STR(?mecanismoLabel) AS ?mecanismo)
  }
}
ORDER BY ?nombreFarmaco
"

resultado5 <- SPARQL(endpoint, query5)

datatable(resultado5$results, 
          caption = "Consulta 5: Fármacos aprobados para Alzheimer",
          options = list(pageLength = 10))

fecha <- format(Sys.Date(), "%Y%m%d")

write.csv(resultado1$results, paste0("consulta1_variantes_alzheimer.csv"), row.names = FALSE)
write.csv(resultado2$results, paste0("consulta2_genes_proteinas_funciones.csv"), row.names = FALSE)
write.csv(resultado3$results, paste0("consulta3_clasificacion_variantes.csv"), row.names = FALSE)
write.csv(resultado4$results, paste0("consulta4_fenotipos_clinicos.csv"), row.names = FALSE)
write.csv(resultado5$results, paste0("consulta5_tratamientos.csv"), row.names = FALSE)