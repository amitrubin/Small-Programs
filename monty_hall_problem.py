from random import randint


def create_random_stage():
    room_of_prize = randint(0, 2)
    empty_rooms = list({0, 1, 2} - {room_of_prize})
    initial_player_guess = randint(0, 2)
    if initial_player_guess == room_of_prize:
        room_revealed_by_host = empty_rooms[randint(0, 1)]
    else:
        room_revealed_by_host = list({0, 1, 2} - {room_of_prize, initial_player_guess})[0]
    return room_of_prize, room_revealed_by_host, initial_player_guess


def random_decision(room_revealed_by_host, initial_player_guess):
    options = list({0, 1, 2} - {room_revealed_by_host})
    return options[randint(0, 1)]


def smart_decision(room_revealed_by_host, initial_player_guess):
    return list({0, 1, 2} - {room_revealed_by_host, initial_player_guess})[0]


def stupid_decision(room_revealed_by_host, initial_player_guess):
    return initial_player_guess


def single_run(decision_policy):
    room_of_prize, room_revealed_by_host, initial_player_guess = create_random_stage()
    guess = decision_policy(room_revealed_by_host, initial_player_guess)
    player_won = guess == room_of_prize
    return player_won


counter_random_decision = 0
counter_smart_decision = 0
counter_stupid_decision = 0


number_of_runs = 1_000_000
for i in range(number_of_runs):
    if single_run(random_decision):
        counter_random_decision += 1
    if single_run(smart_decision):
        counter_smart_decision += 1
    if single_run(stupid_decision):
        counter_stupid_decision += 1

print(f"We run the game show for each decision policy {number_of_runs:,} times.\n"
      f"These are the results:\n"
      f" random policy wins = {counter_random_decision:,}\n"
      f" smart policy wins = {counter_smart_decision:,}\n"
      f" stupid policy wins = {counter_stupid_decision:,}")
