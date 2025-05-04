## load libraries ----
library(tidyverse)
library(openalexR)
library(tidygraph)
library(ggraph)
library(igraph)
library(synthesisr)
## Read in data ----

# lens_data<-read_refs("data/lens-export.ris")
# 
# id_list<-lens_data$doi # one is missing a doi as it is a thesis
# 
# ## Get works from OpenAlex ----
# works <- oa_fetch(
#   entity = "works",
#   doi = id_list,
#   verbose = TRUE
# )
# 
# #save works
#saveRDS(works, "data/works.rds")

works <- readRDS("data/works.rds")

#works |> view()

## Build edges: pairs of concepts that co-occur in the same paper ----

### All concept levels ----

concept_edges <- works |>
  select(id, concepts) |>
  unnest_longer(concepts) |>
  unnest_wider(concepts, names_sep = "_") |>
  select(work_id = id, concept = concepts_display_name) |>
  group_by(work_id) |>
  summarise(concepts = list(concept), .groups = "drop") |>
  filter(lengths(concepts) >= 2) |>  # filter to avoid combn() failure
  pull(concepts) |>
  map(combn, 2, simplify = FALSE) |>
  flatten() |>
  map_df(~ tibble(from = .x[1], to = .x[2])) |>
  count(from, to, sort = TRUE)


# Turn into a graph
concept_graph <- as_tbl_graph(concept_edges, directed = FALSE)

# Plot it
ggraph(concept_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.3) +
  geom_node_point(color = "steelblue", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.2, 2)) +
  theme_void()

### Top concepts ----

top_concept_edges <- works |>
  select(id, concepts) |>
  unnest_longer(concepts) |>
  unnest_wider(concepts, names_sep = "_") |>
  filter(concepts_level == 0) |>
  select(work_id = id, concept = concepts_display_name) |>
  group_by(work_id) |>
  summarise(concepts = list(concept), .groups = "drop") |>
  filter(lengths(concepts) >= 2) |> 
  pull(concepts) |>
  map(combn, 2, simplify = FALSE) |>
  flatten() |>
  map_df(~ tibble(from = .x[1], to = .x[2])) |>
  count(from, to, sort = TRUE)
# Turn into a graph
top_concept_graph <- as_tbl_graph(top_concept_edges, directed = FALSE)

# Plot it
ggraph(top_concept_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.3) +
  geom_node_point(color = "steelblue", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.2, 2)) +
  theme_void()


# first concept level
first_concept_edges <- works |>
  select(id, concepts) |>
  unnest_longer(concepts) |>
  unnest_wider(concepts, names_sep = "_") |>
  filter(concepts_level == 1) |>
  select(work_id = id, concept = concepts_display_name) |>
  group_by(work_id) |>
  summarise(concepts = list(concept), .groups = "drop") |>
  filter(lengths(concepts) >= 2) |> 
  pull(concepts) |>
  map(combn, 2, simplify = FALSE) |>
  flatten() |>
  map_df(~ tibble(from = .x[1], to = .x[2])) |>
  count(from, to, sort = TRUE)

# Turn into a graph
first_concept_graph <- as_tbl_graph(first_concept_edges, directed = FALSE)

# Plot it
ggraph(first_concept_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.3) +
  geom_node_point(color = "steelblue", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.2, 2)) +
  theme_void()


# second concept level
second_concept_edges <- works |>
  select(id, concepts) |>
  unnest_longer(concepts) |>
  unnest_wider(concepts, names_sep = "_") |>
  filter(concepts_level == 2) |>
  select(work_id = id, concept = concepts_display_name) |>
  group_by(work_id) |>
  summarise(concepts = list(concept), .groups = "drop") |>
  filter(lengths(concepts) >= 2) |> 
  pull(concepts) |>
  map(combn, 2, simplify = FALSE) |>
  flatten() |>
  map_df(~ tibble(from = .x[1], to = .x[2])) |>
  count(from, to, sort = TRUE)

# Turn into a graph
second_concept_graph <- as_tbl_graph(second_concept_edges, directed = FALSE)

# Plot it
ggraph(second_concept_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.3) +
  geom_node_point(color = "steelblue", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.2, 2)) +
  theme_void()


# third concept level
third_concept_edges <- works |>
  select(id, concepts) |>
  unnest_longer(concepts) |>
  unnest_wider(concepts, names_sep = "_") |>
  filter(concepts_level == 3) |>
  select(work_id = id, concept = concepts_display_name) |>
  group_by(work_id) |>
  summarise(concepts = list(concept), .groups = "drop") |>
  filter(lengths(concepts) >= 2) |> 
  pull(concepts) |>
  map(combn, 2, simplify = FALSE) |>
  flatten() |>
  map_df(~ tibble(from = .x[1], to = .x[2])) |>
  count(from, to, sort = TRUE)

# Turn into a graph
third_concept_graph <- as_tbl_graph(third_concept_edges, directed = FALSE)

# Plot it
ggraph(third_concept_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.3) +
  geom_node_point(color = "steelblue", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.2, 2)) +
  theme_void()
# fourth concept level
fourth_concept_edges <- works |>
  select(id, concepts) |>
  unnest_longer(concepts) |>
  unnest_wider(concepts, names_sep = "_") |>
  filter(concepts_level == 4) |>
  select(work_id = id, concept = concepts_display_name) |>
  group_by(work_id) |>
  summarise(concepts = list(concept), .groups = "drop") |>
  filter(lengths(concepts) >= 2) |> 
  pull(concepts) |>
  map(combn, 2, simplify = FALSE) |>
  flatten() |>
  map_df(~ tibble(from = .x[1], to = .x[2])) |>
  count(from, to, sort = TRUE)

# Turn into a graph
fourth_concept_graph <- as_tbl_graph(fourth_concept_edges, directed = FALSE)

# Plot it
ggraph(fourth_concept_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.3) +
  geom_node_point(color = "steelblue", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.2, 2)) +
  theme_void()


### Co-citation network----

# Extract all referenced works
ref_pairs <- works |>
  select(id, referenced_works) |>
  drop_na() |>
  mutate(n_refs = lengths(referenced_works)) |>
  filter(n_refs >= 2) |>
  pull(referenced_works) |>
  map(combn, 2, simplify = FALSE) |>
  flatten() |>
  map_df(~ tibble(from = .x[1], to = .x[2])) |>
  count(from, to, sort = TRUE)


# Convert to graph
#cocit_graph <- as_tbl_graph(ref_pairs, directed = FALSE)

# # Visualize Too BIG
# ggraph(cocit_graph, layout = "fr") +
#   geom_edge_link(aes(width = n), alpha = 0.3) +
#   geom_node_point(color = "tomato", size = 3) +
#   geom_node_text(aes(label = name), repel = TRUE, size = 2.5) +
#   theme_void()



# Only keep edges with ??? 3 co-citations
filtered_edges <- ref_pairs |> filter(n >= 3)

# Keep only nodes involved in those edges
filtered_node_ids <- unique(c(filtered_edges$from, filtered_edges$to))


node_ids <- unique(c(ref_pairs$from, ref_pairs$to))
#ref_metadata <- oa_fetch(
#  entity = "works",
#  openalex_id = node_ids,
#  verbose = TRUE
#)

#saveRDS(ref_metadata, "data/ref_metadata.rds")
ref_metadata <- readRDS("data/ref_metadata.rds")

filtered_nodes <- ref_metadata |> 
  filter(id %in% filtered_node_ids) |> 
  select(id, title, publication_year, cited_by_count) |> 
  rename(name = id)

cocit_graph_small <- tbl_graph(
  nodes = filtered_nodes,
  edges = filtered_edges,
  directed = FALSE
)

top_nodes <- ref_metadata |> 
  slice_max(cited_by_count, n = 100) |> 
  pull(id)

ref_pairs_top <- ref_pairs |> 
  filter(from %in% top_nodes & to %in% top_nodes)

cocit_graph <- cocit_graph_small |> 
  mutate(component = group_components()) |> 
  filter(component == 1)  # Largest connected component
cocit_graph <- cocit_graph |> 
  mutate(label = ifelse(cited_by_count >= 100, str_trunc(title, 40), NA))

ggraph(cocit_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.2) +
  geom_node_point(aes(size = cited_by_count), color = "tomato") +
  geom_node_text(aes(label = label), repel = TRUE, size = 3, na.rm = TRUE) +
  theme_void()


##### 1. Prepare a list of each work and its references
ref_lists <- works |>
  select(id, referenced_works) |>
  drop_na()

# 2. Create all unique work pairs
work_pairs <- combn(ref_lists$id, 2, simplify = FALSE) |> 
  map_dfr(~ {
    a <- ref_lists$referenced_works[ref_lists$id == .x[1]][[1]]
    b <- ref_lists$referenced_works[ref_lists$id == .x[2]][[1]]
    shared <- length(intersect(a, b))
    tibble(from = .x[1], to = .x[2], shared_refs = shared)
  })

# 3. Filter to retain only strong links (adjust threshold)
biblio_edges <- work_pairs |> 
  filter(shared_refs >= 2)

biblio_nodes <- works |> 
  filter(id %in% unique(c(biblio_edges$from, biblio_edges$to))) |> 
  select(id, title, cited_by_count) |> 
  rename(name = id)

biblio_graph <- tbl_graph(nodes = biblio_nodes, edges = biblio_edges, directed = FALSE)

ggraph(biblio_graph, layout = "fr") +
  geom_edge_link(aes(width = shared_refs), alpha = 0.3) +
  geom_node_point(color = "steelblue", size = 3) +
  geom_node_text(aes(label = str_trunc(title, 30)), size = 2.5, repel = TRUE) +
  theme_void()


###
author_pairs <- works |>
  select(id, author) |>
  unnest_longer(author) |>
  unnest_wider(author) |>
  select(id, au_display_name) |>
  group_by(id) |>
  summarise(authors = list(au_display_name), .groups = "drop") |>
  filter(lengths(authors) >= 2) |>
  pull(authors) |>
  map(combn, 2, simplify = FALSE) |>
  flatten() |>
  map_df(~ tibble(from = .x[1], to = .x[2])) |>
  count(from, to, sort = TRUE)

(author_nodes <- tibble(name = unique(c(author_pairs$from, author_pairs$to))))

author_graph <- tbl_graph(
  nodes = author_nodes,
  edges = author_pairs |> select(from, to, weight = n),
  directed = FALSE
)

ggraph(author_graph, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.3) +
  geom_node_point(color = "darkorange", size = 3) +
  geom_node_text(aes(label = name), size = 2.5, repel = TRUE) +
  theme_void()
