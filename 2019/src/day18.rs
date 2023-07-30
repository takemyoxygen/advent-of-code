use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};

use itertools::Itertools;

use crate::{solver::Day, utils};

pub struct Day18;

const STEPS: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, 1), (0, -1)];

type Map = Vec<Vec<char>>;

fn find(map: &Map, expected: &char) -> Option<(usize, usize)> {
    for i in 0..map.len() {
        for j in 0..map[i].len() {
            if &map[i][j] == expected {
                return Some((i, j));
            }
        }
    }

    None
}

fn adjacent_to(i: usize, j: usize) -> Vec<(usize, usize)> {
    STEPS
        .iter()
        .map(|(dx, dy)| (i as i32 + dx, j as i32 + dy))
        .filter(|(x, y)| *x >= 0 && *y >= 0)
        .map(|(x, y)| (x as usize, y as usize))
        .collect()
}

fn is_key(c: &char) -> bool {
    return c.is_lowercase();
}

fn is_door(c: &char) -> bool {
    return c.is_uppercase();
}

fn convert_to_graph(
    map: &Map,
    start: (usize, usize),
) -> HashMap<char, HashSet<(char, usize, usize)>> /* node -> (node, dist, keys) */
{
    let mut pois = VecDeque::from([start]);
    let mut visited_pois = HashSet::new();
    let mut graph = HashMap::new();

    while !pois.is_empty() {
        let poi = pois.pop_front().unwrap();
        let poi_char = map[poi.0][poi.1].clone();

        if !visited_pois.insert(poi.clone()) {
            continue;
        }

        let mut queue = VecDeque::from([(poi, 0)]);
        let mut visited = HashSet::new();

        while !queue.is_empty() {
            let ((i, j), dist) = queue.pop_front().unwrap();
            if !visited.insert((i, j)) {
                continue;
            }

            let char = map[i][j].clone();
            if (is_key(&char) || is_door(&char)) && dist > 0 {
                let keys_required = if is_door(&char) {
                    char_to_mask(&char)
                } else {
                    0
                } | if is_door(&poi_char) {
                    char_to_mask(&poi_char)
                } else {
                    0
                };
                let edge1 = (poi_char, dist, keys_required);
                graph
                    .entry(char)
                    .and_modify(|v: &mut HashSet<(char, usize, usize)>| {
                        v.insert(edge1);
                    })
                    .or_insert(HashSet::from([edge1]));

                let edge2 = (char, dist, keys_required);

                graph
                    .entry(poi_char)
                    .and_modify(|v: &mut HashSet<(char, usize, usize)>| {
                        v.insert(edge2);
                    })
                    .or_insert(HashSet::from([edge2]));

                if !visited_pois.contains(&(i, j)) {
                    pois.push_back((i, j));
                }
                continue;
            }

            for (x, y) in adjacent_to(i, j).iter().filter(|(x, y)| {
                map.get(*x)
                    .and_then(|row| row.get(*y))
                    .map(|c| c != &'#')
                    .unwrap_or(false)
                    && !visited.contains(&(*x, *y))
            }) {
                queue.push_back(((*x, *y), dist + 1))
            }
        }
    }

    graph
}

fn char_to_mask(c: &char) -> usize {
    let c = c.to_lowercase().next().unwrap();
    let code = (c as usize) - ('a' as usize);
    return 1 << code;
}

impl Day for Day18 {
    type Intermediate = Map;

    fn process(file: &std::path::Path) -> Self::Intermediate {
        utils::read_lines(file)
            .map(|s| s.split("").filter_map(|c| c.chars().next()).collect())
            .collect()
    }

    fn part1(map: &Self::Intermediate) -> String {
        let start = find(map, &'@').unwrap();

        let all_keys = map
            .iter()
            .flatten()
            .filter(|c| is_key(c))
            .map(|c| char_to_mask(&c))
            .reduce(|a, b| a | b)
            .unwrap();

        let init: (usize, usize, usize) = (start.0, start.1, 0);
        let mut queue = BinaryHeap::new();
        queue.push((0, init));
        let mut distances = HashMap::new();
        distances.insert(init, 0 as usize);

        let mut best = usize::MAX;

        while let Some((dist, pos)) = queue.pop() {
            let (i, j, keys) = pos;

            let prev_dist = distances.get(&pos).unwrap_or(&usize::MAX);
            if prev_dist < &dist {
                continue;
            }

            let current = map[i][j];
            let next_keys = if is_key(&current) {
                keys | char_to_mask(&current)
            } else {
                keys
            };

            if next_keys == all_keys {
                best = usize::min(best, dist);
                continue;
            }

            let next_dist = dist + 1;

            for (ai, aj) in adjacent_to(i, j).iter().filter(|(ai, aj)| {
                let adj_char = map[*ai][*aj];
                return adj_char != '#'
                    && (!is_door(&adj_char) || next_keys & char_to_mask(&adj_char) > 0);
            }) {
                let next_pos = (*ai, *aj, next_keys);
                let prev_dist = distances.get(&next_pos).unwrap_or(&usize::MAX);
                if next_dist < *prev_dist {
                    distances.insert(next_pos, next_dist);
                    queue.push((next_dist, next_pos));
                }
            }
        }

        best.to_string()
    }

    fn part2(map: &Self::Intermediate) -> String {
        let mut map = map.clone();
        let (mid_i, mid_j) = find(&map, &'@').unwrap();
        map[mid_i][mid_j] = '#';
        for (adj_i, adj_j) in adjacent_to(mid_i, mid_j).iter() {
            map[*adj_i][*adj_j] = '#';
        }

        let starts = [
            ((mid_i - 1, mid_j - 1), '1'),
            ((mid_i - 1, mid_j + 1), '2'),
            ((mid_i + 1, mid_j - 1), '3'),
            ((mid_i + 1, mid_j + 1), '4'),
        ];

        for ((i, j), c) in starts {
            map[i][j] = c;
        }

        let graph = starts
            .iter()
            .map(|(st, _)| convert_to_graph(&map, *st))
            .reduce(|m1, m2| {
                let mut result = m1.clone();
                result.extend(m2);
                return result;
            })
            .unwrap();

        for (node, edges) in graph.iter() {
            println!(
                "{} -> {}",
                node,
                edges.iter().map(|e| format!("{:?}", e)).join(", ")
            );
        }

        let all_keys = map
            .iter()
            .flatten()
            .filter(|c| is_key(c))
            .map(|c| char_to_mask(&c))
            .reduce(|a, b| a | b)
            .unwrap();

        println!("All keys: {}", all_keys);

        let initial = (starts.map(|(_, c)| c), 0 as usize);
        let mut queue = BinaryHeap::new();
        queue.push((0 as i32, initial));
        let mut distances = HashMap::new();
        distances.insert(initial, 0);

        let mut best = i32::max_value();

        while let Some((neg_dist, pos)) = queue.pop() {
            let dist = -neg_dist;
            let prev_dist = distances.get(&pos).unwrap_or(&i32::max_value());
            if prev_dist < &dist {
                continue;
            }

//            println!("Current state: {:?} (dist = {})", pos, dist);

            let (nodes, keys) = pos;

            let next_keys = nodes
                .iter()
                .map(|c| if is_key(c) { char_to_mask(c) } else { 0 })
                .fold(keys, |a, b| a | b);

            if all_keys == next_keys {
                if dist < best {
                    println!("Got new best: {}", dist);
                }
                best = i32::min(dist, best);
            }

            for idx in 0..nodes.len() {
                for (adj, edge_dist, keys_req) in graph.get(&nodes[idx]).unwrap_or(&HashSet::new())
                {
                    if keys_req == &0 || next_keys & keys_req > 0 {
                        let mut next_nodes = nodes.clone();
                        next_nodes[idx] = *adj;
                        let next_state = (next_nodes, next_keys);
                        let next_dist = dist + (*edge_dist as i32);
                        if next_dist < *distances.get(&next_state).unwrap_or(&i32::max_value()) {
//                            println!("Enque: {:?} (dist: {})", next_state, next_dist);
                            distances.insert(next_state, next_dist);
                            queue.push((-(next_dist as i32), next_state));
                        }
                    }
                }
            }
        }

        best.to_string()
    }
}
