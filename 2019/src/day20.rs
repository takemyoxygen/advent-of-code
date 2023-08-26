use std::collections::{HashMap, VecDeque, HashSet};

use crate::{solver::Day, utils};

const STEPS: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, 1), (0, -1)];

type PortalsByCoord = HashMap<(usize, usize), Vec<String>>;
type PortalsByLabel = HashMap<String, Vec<(usize, usize)>>;

fn adj_positions(x: &usize, y: &usize) -> Vec<(usize, usize)> {
    return STEPS
        .iter()
        .map(|(dx, dy)| ((*x as i32) + dx, (*y as i32) + dy))
        .filter(|(adj_x, adj_y)| adj_x >= &0 && adj_y >= &0)
        .map(|(adj_x, adj_y)| (adj_x as usize, adj_y as usize))
        .collect();
}

pub struct Day20;

impl Day for Day20 {
    type Intermediate = (Vec<Vec<char>>, PortalsByLabel, PortalsByCoord);

    fn process(file: &std::path::Path) -> Self::Intermediate {
        let lines: Vec<Vec<char>> = utils::read_lines(file).map(|line| line.chars().collect()).collect();
        let mut portals_by_label: PortalsByLabel = HashMap::new();
        let mut portals_by_coord: PortalsByCoord = HashMap::new();

        for y in 0..lines.len() - 1 {
            for x in 0..lines[y].len() - 1 {
                if lines[y][x].is_ascii_alphabetic() {
                    let adj = adj_positions(&x, &y);
                    let adj_space = adj.iter().find(|(ax, ay)| lines[*ay][*ax] == '.');
                    if adj_space.is_some() {
                        let another_letter_pos = adj.iter().find(|(ax, ay)| lines[*ay][*ax].is_ascii_alphabetic()).unwrap();
                        let mut letters = vec![(x, y), *another_letter_pos];
                        letters.sort();
                        
                        let portal_label = letters.iter().map(|(x, y)| lines[*y][*x]).collect::<String>();
                        let portal_position = *adj_space.unwrap();
                        
                        portals_by_label.entry(portal_label.clone()).or_insert(vec![]).push(portal_position);
                        portals_by_coord.entry(portal_position).or_insert(vec![]).push(portal_label);
                    }
                }
            }
        }

        (lines, portals_by_label, portals_by_coord)
    }

    fn part1(input: &Self::Intermediate) -> String {
        let (grid, portals_by_label, portals_by_coord) = input;
        let start = portals_by_label.get("AA").unwrap()[0];
        let dest = portals_by_label.get("ZZ").unwrap()[0];

        let mut queue = VecDeque::from([(start, 0)]);
        let mut visited: HashSet<(usize, usize)> = HashSet::new();

        while queue.len() > 0 {
            let (pos, length) = queue.pop_front().unwrap();
            if pos == dest {
                return length.to_string();
            }
            
            visited.insert(pos);
            
            let (x, y) = pos;
            
            for adj in adj_positions(&x, &y).iter().filter(|(ax, ay)| grid[*ay][*ax] == '.' && !visited.contains(&(*ax, *ay))) {
                queue.push_back((*adj, length + 1))
            }
            
            for portal_locaction in portals_by_coord
                .get(&pos)
                .unwrap_or(&vec![])
                .iter()
                .flat_map(|portal| portals_by_label.get(portal).unwrap())
                .filter(|portal_dest| *portal_dest != &pos) {
                queue.push_back((*portal_locaction, length + 1))
            }
        }
        
        panic!("Path not found");
    }

    fn part2(input: &Self::Intermediate) -> String {
        let (grid, portals_by_label, portals_by_coord) = input;
        let start = portals_by_label.get("AA").unwrap()[0];
        let dest = portals_by_label.get("ZZ").unwrap()[0];

        let mut queue = VecDeque::from([(start, 0, 0)]);
        let mut visited: HashSet<((usize, usize), usize)> = HashSet::new();

        while queue.len() > 0 {
            let (pos, length, level) = queue.pop_front().unwrap();
            if pos == dest && level == 0 {
                return length.to_string();
            }

            visited.insert((pos, level));

            let (x, y) = pos;

            for adj in adj_positions(&x, &y)
                .iter()
                .filter(|(ax, ay)|
                    grid[*ay][*ax] == '.' &&
                    (level == 0 || ((*ax, *ay) != start && (*ax, *ay) != dest)) &&
                    !visited.contains(&((*ax, *ay), level))) {
                queue.push_back((*adj, length + 1, level))
            }
            
            let is_outer = x == 2 || y == 2 || y == grid.len() - 3 || x == grid[y].len() - 3;
            
            if is_outer && level == 0 {
                continue;
            }
            
            for portal_locaction in portals_by_coord
                .get(&pos)
                .unwrap_or(&vec![])
                .iter()
                .flat_map(|portal| portals_by_label.get(portal).unwrap())
                .filter(|portal_dest| *portal_dest != &pos) {
                let next_level = if is_outer {level - 1} else {level + 1};
                queue.push_back((*portal_locaction, length + 1, next_level));
            }
        }

        panic!("Path not found");
    }
}
