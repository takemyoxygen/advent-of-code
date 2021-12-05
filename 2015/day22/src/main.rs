use std::cmp;

trait Alive {
    fn receive_damage(&mut self, damage: usize);
}

#[derive(Debug, Clone)]
struct Boss {
    health: usize,
    damage: usize
}

impl Alive for Boss {
    fn receive_damage(&mut self, damage: usize) {
        self.health -= std::cmp::min(damage, self.health);
    }
}

impl Boss {
    fn new() -> Boss {
        Boss { health: 58, damage: 9 }
    }
}

#[derive(Debug, Clone)]
struct Player {
    health: usize,
    armor: usize,
    mana: usize,
    active_spells: Vec<Spell>
}

impl Alive for Player {
    fn receive_damage(&mut self, damage: usize) {
        self.health -= std::cmp::min(damage, self.health);
    }
}

impl Player {
    fn new() -> Player {
        Player { health: 50, armor: 0, mana: 500, active_spells: Vec::new() }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum SpellType {
    MagicMissile,
    Drain,
    Shield,
    Poison,
    Recharge
}

#[derive(Clone, Debug)]
struct Spell {
    mana: usize,
    spell_type: SpellType,
    timer: usize
}

impl Spell {

    fn all() -> Vec<Spell> {
        vec![
            Spell { mana: 53, spell_type: SpellType::MagicMissile, timer: 1 },
            Spell { mana: 73, spell_type: SpellType::Drain, timer: 1 },
            Spell { mana: 113, spell_type: SpellType::Shield, timer: 6 },
            Spell { mana: 173, spell_type: SpellType::Poison, timer: 6 },
            Spell { mana: 229, spell_type: SpellType::Recharge, timer: 5 },
        ]
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum Difficulty {
    Normal,
    Hard
}

fn apply_effects(player: &mut Player, boss: &mut Boss) {
    for spell in player.active_spells.iter_mut() {
        spell.timer -=1;

        match spell.spell_type {
            SpellType::MagicMissile => boss.receive_damage(4),
            SpellType::Drain => { player.health += 2; boss.receive_damage(2); }
            SpellType::Shield => player.armor = 7,
            SpellType::Poison => boss.receive_damage(3),
            SpellType::Recharge => player.mana += 101,
        }
    }

    for i in (0..player.active_spells.len()).rev() {
        if player.active_spells[i].timer == 0 {
            player.active_spells.remove(i);
        }
    }
}

fn boss_turn(player: &mut Player, boss: &Boss) {
    let damage = if boss.damage > player.armor { boss.damage - player.armor } else { 1 };
    player.receive_damage(damage);
}

fn pick_available_spells(player: &Player, spells: &Vec<Spell>) -> Vec<Spell> {
    spells
        .iter()
        .filter(|spell| player.mana >= spell.mana )
        .filter(|spell| player.active_spells.iter().all(|s| s.spell_type != spell.spell_type))
        .cloned()
        .collect()
}

fn round(
    player: Player,
    boss: Boss,
    spells: &Vec<Spell>,
    mana_spent_so_far: usize,
    best_result_so_far: usize,
    difficulty: Difficulty) -> usize {

    if player.health == 0 { best_result_so_far }
    else {
        let mut player = player;
        let mut boss = boss;
        player.armor = 0;

        if difficulty == Difficulty::Hard {
            player.receive_damage(1);
            if player.health == 0 {
                return best_result_so_far;
            }
        }

        apply_effects(&mut player, &mut boss);

        if boss.health == 0 { cmp::min(best_result_so_far, mana_spent_so_far) }
        else {
            pick_available_spells(&mut player, spells)
                .iter()
                .fold(best_result_so_far, |best_result, spell|{
                    let mut player = player.clone();
                    let mut boss = boss.clone();

                    player.mana -= spell.mana;
                    player.active_spells.push(spell.clone());
                    let mana_spent_so_far = mana_spent_so_far + spell.mana;

                    if best_result <= mana_spent_so_far {
                        best_result
                    } else {
                        player.armor = 0;
                        apply_effects(&mut player, &mut boss);
                        if boss.health > 0 {
                            boss_turn(&mut player, &mut boss);
                            round(player, boss, spells, mana_spent_so_far, best_result, difficulty)
                        } else {
                            mana_spent_so_far
                        }
                    }
                })
        }
    }
}

fn main() {
    let spells = Spell::all();

    let mana_spent = round(Player::new(), Boss::new(), &spells, 0, usize::max_value(), Difficulty::Normal);
    println!("Part 1: min amount of mana to win: {:?}", mana_spent);

    let mana_spent = round(Player::new(), Boss::new(), &spells, 0, usize::max_value(), Difficulty::Hard);
    println!("Part 2: min amount of mana to win: {:?}", mana_spent);
}
