#[derive(Debug)]
struct Item {
    name: String,
    price: usize,
    damage: usize,
    armor: usize,
}

impl Item {
    fn new(name: &str, price: usize, damage: usize, armor: usize) -> Item {
        Item { name: name.to_string(), price: price, damage: damage, armor: armor }
    }
}

fn players_wins(inventory: &[&Item]) -> bool {
    let enemy_hit_points = 109;
    let enemy_damage = 8;
    let enemy_armor = 2;

    let player_hit_points = 100;
    let player_damage = inventory.iter().map(|item| item.damage).fold(0, |acc, x| acc + x);
    let player_armor = inventory.iter().map(|item| item.armor).fold(0, |acc, x| acc + x);

    let actual_player_damage = if player_damage > enemy_armor { player_damage - enemy_armor } else { 1 };
    let actual_enemy_damage = if enemy_damage > player_armor { enemy_damage - player_armor } else { 1 };

    let rounds_for_player_to_win = 1 + (enemy_hit_points - 1) / actual_player_damage;
    let rounds_for_enemy_to_win = 1 + (player_hit_points - 1) / actual_enemy_damage;

    rounds_for_player_to_win <= rounds_for_enemy_to_win
}

fn total_price(inventory: &[&Item]) -> usize {
    inventory.iter().map(|item| item.price).fold(0, |acc, x| acc + x)
}

fn main() {
    let weapons = [
        Item::new("Dagger", 8, 4, 0),
        Item::new("Shortsword", 10, 5, 0),
        Item::new("Warhammer", 25, 6, 0),
        Item::new("Longsword", 40, 7, 0),
        Item::new("Greataxe", 74, 8, 0)
     ];

    let armors = [
         Item::new("Leather", 13, 0, 1),
         Item::new("Chainmail", 31, 0, 2),
         Item::new("Splintmail", 53, 0, 3),
         Item::new("Bandedmail", 75, 0, 4),
         Item::new("Platemail", 102, 0, 5),
         Item::new("No armor", 0, 0, 0)
     ];

    let rings = [
        Item::new("Damage +1", 25, 1, 0),
        Item::new("Damage +2", 50, 2, 0),
        Item::new("Damage +3", 100, 3, 0),
        Item::new("Defense +1", 20, 0, 1),
        Item::new("Defense +2", 40, 0, 2),
        Item::new("Defense +3", 80, 0, 3),
        Item::new("No ring #1", 0, 0, 0),
        Item::new("No ring #2", 0, 0, 0)
    ];


    let mut min_price = usize::max_value();
    let mut max_price = usize::min_value();

    for weapon in weapons.iter() {
        for armor in armors.iter() {
            for r1 in 0..rings.len()-1 {
                for r2 in r1+1..rings.len() {
                    let inventory = [weapon, armor, &rings[r1], &rings[r2]];
                    let price = total_price(&inventory);
                    let player_wins = players_wins(&inventory);

                    if player_wins && price < min_price {
                        min_price = price;
                    } else if !player_wins && price > max_price {
                        max_price = price;
                    }
                }
            }
        }
    }

    println!("Smallest amount to win: {}", min_price);
    println!("Biggest amount to lose: {}", max_price);
}
