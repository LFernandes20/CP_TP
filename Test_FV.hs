--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 3 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também indentificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: A74859
-- Nome: João da Cunha Coelho
-- Curso: Mestrado Integrado Engenharia Informática
--
-- Aluno 2
-- Número: A74748
-- Nome: Luís Miguel Moreira Fernandes
-- Curso: Mestrado Integrado Engenharia Informática
--


module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set
import Data.Maybe

--
-- Teste unitário
--
    
g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }

g2 :: Graph Int
g2 = Graph {nodes = fromList [1, 2],
            edges = fromList [Edge 1 1, Edge 1 2]
            }

g3 :: Graph Int
g3 = Graph.empty

g4 :: Graph Int
g4 = Graph {nodes = fromList [1, 2, 3],
            edges = fromList [Edge 1 2, Edge 1 5, Edge 2 3]
           }

g5 :: Graph Int
g5 = Graph {nodes = fromList [1, 2, 3, 4, 5],
            edges = fromList [Edge 1 2, Edge 2 4, Edge 4 5, Edge 5 1, Edge 2 3]
           }

g6 :: Graph Int
g6 = Graph {nodes = fromList [1, 2, 3, 4, 5],
            edges = fromList [Edge 1 3, Edge 2 4, Edge 4 5]
           }

g7 :: Graph Int
g7 = Graph {nodes = fromList [1, 2, 3, 4, 5],
            edges = fromList [Edge 3 1, Edge 4 2, Edge 5 4]
           }

g8 :: Graph Int
g8 = Graph {nodes = fromList [1, 2, 3, 4, 5],
            edges = fromList [Edge 1 1, Edge 1 2, Edge 1 3, Edge 2 4, Edge 4 5]
           }

g9 :: Graph Int
g9 = Graph {nodes = fromList [1, 2, 3, 4, 5, 6],
            edges = fromList [Edge 1 3, Edge 1 5, Edge 2 5, Edge 3 4, Edge 4 2, Edge 5 6]
           }

g10 :: Graph Int
g10 = Graph {nodes = fromList [1, 2, 3],
             edges = fromList [Edge 1 2, Edge 1 3]
            }

g11 :: Graph Int
g11 = Graph {nodes = fromList [1, 2, 3],
             edges = fromList [Edge 1 2, Edge 1 3, Edge 1 2]
            }


s1 :: Set Int
s1 = fromList [2]

s2 :: Set Int
s2 = fromList []

s3 :: Set Int
s3 = fromList [1, 2]


f1 :: Forest Int
f1 = Graph {nodes = fromList [2,5,6],
            edges = fromList [Edge {source = 5, target = 2}, Edge {source = 6, target = 5}]
           }

f2 :: Forest Int
f2 = Graph {nodes = fromList [], edges = fromList []}


--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--

test_swap :: Test
test_swap = TestList [swap (Edge 1 1) ~?= (Edge 1 1),
                      swap (Edge 1 3) ~?= (Edge 3 1)
                     ]

test_isEmpty :: Test
test_isEmpty = TestList [isEmpty (g1) ~?= False,
                         isEmpty (g2) ~?= False,
                         isEmpty (g3) ~?= True
                        ]

test_isValid :: Test
test_isValid = TestList [isValid (g1) ~?= True,
                         isValid (g3) ~?= True,
                         isValid (g4) ~?= False,
                         isValid (g11) ~?= True    -- Será um grafo com duas arestas entre os mesmos vértices válido?
                        ]

test_isDAG :: Test
test_isDAG = TestList [isDAG (g1) ~?= False,    -- Um grafo só de um nodo é acíclico?
                       isDAG (g2) ~?= False,    -- Mesmo problema do de cima.
                       isDAG (g3) ~?= True,
                       isDAG (g5) ~?= False,
                       isDAG (g9) ~?= True
                      ]

test_isForest :: Test
test_isForest = TestList [isForest (g6) ~?= True,
                          isForest (g10) ~?= False
                         ]

test_isSubgraphOf :: Test
test_isSubgraphOf = TestList [isSubgraphOf (g1) (g2) ~?= True,
                              isSubgraphOf (g2) (g5) ~?= False,
                              isSubgraphOf (g3) (g6) ~?= True,       -- Será um grafo vazio sugGrafo de qualquer outro?
                              isSubgraphOf (g1) (g3) ~?= False
                             ]

test_adj :: Test
test_adj = TestList [adj (g1) 1 ~?= fromList [Edge 1 1],
                     adj (g2) 1 ~?= fromList [Edge 1 1, Edge 1 2],
                     adj (g8) 1 ~?= fromList [Edge 1 1, Edge 1 2, Edge 1 3]
                    ]

test_transpose :: Test
test_transpose = TestList [transpose (g1) ~?= g1,
                           transpose (g3) ~?= g3,
                           transpose (g6) ~?= g7
                          ]

test_union :: Test
test_union = TestList [Graph.union (g1) (g3) ~?= g1,
                       Graph.union (g2) (g6) ~?= g8
                      ]

test_bft :: Test
test_bft = TestList [bft (g9) (s1) ~?= f1,
                     bft (g9) (s2) ~?= f2]

test_reachable :: Test
test_reachable = TestList [reachable (g5) 1 ~?= fromList [1, 2, 3, 4, 5],
                           reachable (g5) 3 ~?= fromList [3],                 -- Um vértice é reachable a partir dele próprio sem ir a mais nenhum?
                           reachable (g6) 4 ~?= fromList [4, 5],
                           reachable (g3) 6 ~?= fromList [6]                   -- Como é que um vértice que não existe chega a algum lado?
                          ]

test_isPathOf :: Test
test_isPathOf = TestList [isPathOf [] (g5) ~?= True,
                          isPathOf [Edge 1 2, Edge 2 4] (g8) ~?= True,
                          isPathOf [Edge 2 4, Edge 4 5, Edge 5 3] (g5) ~?= False
                         ]

test_path :: Test
test_path = TestList [path (g5) 5 1 ~?= Just [Edge 5 1],
                      path (g7) 2 3 ~?= Nothing,
                      path (g8) 1 4 ~?= Just [Edge 1 2, Edge 2 4],
                      path (g9) 1 6 ~?= Just [Edge 1 5, Edge 5 6],
                      path (g9) 1 1 ~?= Just []
                     ]

test_topo :: Test
test_topo = TestList [topo (g3) ~?= [],
                      topo (g9) ~?= [fromList[1], fromList[3], fromList[4], fromList[2], fromList[5], fromList[6]],
                      topo (g10) ~?= [fromList[1], fromList[2, 3]]]
           
main = runTestTT $ TestList [test_adj, test_swap, test_isEmpty, test_isValid, test_isDAG, test_isForest, test_isSubgraphOf, test_transpose,
                             test_union, test_bft, test_reachable, test_isPathOf, test_path, test_topo]

--
-- Teste aleatório
--



--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--

-- Instância de Arbitrary para arestas
instance Arbitrary v => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary
                   return $ Edge {source = s, target = t}

instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
    arbitrary = do ns <- arbitrary
                   if (ns == [])
                   then return $ Graph {nodes = fromList [], edges = fromList []
                                       }
                   else do let i = length ns
                           qtd <- choose (0, (i * (i - 1)) `div` 2)
                           es <- randomEdges qtd ns
                           return $ Graph {nodes = fromList ns, edges = fromList es
                                          }

                           where randomEdges 0 ns' = return []
                                 randomEdges v ns' = do s <- elements ns'
                                                        t <- elements ns'
                                                        resto <- randomEdges (v - 1) ns'
                                                        return ((Edge s t):resto)

prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = do d <- arbitrary
         return Graph {nodes = nodes d, edges = fromList $ geraEdgesDAG (elems (edges d))}

    where geraEdgesDAG :: Ord v => [Edge v] -> [Edge v]
          geraEdgesDAG [] = []
          geraEdgesDAG ((Edge s t):tail) = case (s >= t) of
                                                  True -> geraEdgesDAG (tail)
                                                  False -> (Edge s t):(geraEdgesDAG (tail))

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = do f <- dag
            return Graph {nodes = nodes f, edges = fromList $ geraEdgesForest (elems (edges f))}

    where geraEdgesForest :: Ord v => [Edge v] -> [Edge v]
          geraEdgesForest [] = []
          geraEdgesForest ((Edge s t):tail) = (Edge s t): geraEdgesForest (aux s tail)

          aux :: Eq v => v -> [Edge v] -> [Edge v]
          aux _ [] = []
          aux s ((Edge o d):tail) = case (s == o) of
                                          True -> aux s tail
                                          False -> (Edge o d) : tail

prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g


prop_swap :: Eq v => Edge v -> Bool
prop_swap e = target (swap (swap e)) == target e && source (swap (swap e)) == source e

prop_isEmpty :: Graph Int -> Bool
prop_isEmpty g = case (isEmpty g) of
                            True -> Set.null (nodes g)
                            False -> length (elems (nodes g)) /= 0

prop_isValid :: Graph Int -> Bool
prop_isValid g = case (isValid g) of
                            True -> (Set.map source (edges g) `Set.union` Set.map target (edges g)) `isSubsetOf` nodes g
                            False -> (length (Set.filter (\t -> target t `notMember` (nodes g)) $ edges g) + length (Set.filter(\s -> source s `notMember` (nodes g)) $ edges g)) /= 0

prop_isDAG :: (Show v, Ord v) => Graph v -> Property
prop_isDAG g =  if (size (nodes g) <= 1 || size (edges g) <= 1) 
                then property True
                else size (edges g) > 0 ==> forAll (elements $ elems $ edges g) (\v -> source v < target v)

prop_isForest :: (Ord v, Show v) => DAG v -> Property
prop_isForest g = forAll (elements $ elems $ nodes g) (\v -> length (adj g v) <= 1)

prop_isSubgraphOf :: Graph Int -> Property
prop_isSubgraphOf g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g

prop_transpose :: Ord v => Graph v -> Bool
prop_transpose g = length (nodes g) == length (nodes (transpose g)) && length (edges g) == length (edges (transpose g))

prop_union :: Ord v => Graph v -> Graph v -> Bool
prop_union g g' = all (`member` (nodes (Graph.union g g'))) (nodes g `Set.union` nodes g') && all (`member` (edges (Graph.union g g'))) (edges g `Set.union` edges g')

prop_bft :: Graph Int -> Property
prop_bft g = if size (nodes g) == 0 
             then property True
             else forAll (elements $ elems $ nodes g) (\n -> isForest (bft g (singleton n)))

prop_reachable :: (Ord v, Show v) => Graph v -> Property
prop_reachable g = (size (nodes g) > 0) ==> forAll (elements $ elems $ nodes g) (\v -> nodes ( bft g (singleton v)) `isSubsetOf` (nodes g))

prop_isPathOf :: (Ord v, Show v) => Graph.Path v -> Graph v -> Property
prop_isPathOf p g = (size (Set.map source (edges g) `Set.union` Set.map target (edges g)) > 0 && p /= []) ==> forAll (elements $ p) (`member` edges g)

prop_path :: Graph Int -> Int -> Int -> Property
prop_path g o d = if (size (nodes g) == 0 || (o == d && (path g o d) /= Nothing) || (path g o d == Nothing && d `notMember` reachable g o))
                  then property True
                  else property (length (fromJust (path g o d)) > 0)

prop_topo :: Ord v => DAG v -> Property
prop_topo g = property (if (size (nodes g) <= 1) 
                        then True 
                        else (sum (Prelude.map (size) (topo g)) == (size (nodes g))) && all (\v -> size (adj g v) <= (length (topo g)-1)) (nodes g))