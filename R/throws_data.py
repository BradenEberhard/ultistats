import pandas as pd
import requests
# from audl.stats.endpoints.gameevents import *
# from tqdm import tqdm
# import warnings
# warnings.simplefilter(action='ignore', category=FutureWarning)

def fetch_game_data():
    base_url = "https://www.backend.ufastats.com/api/v1/"
    endpoint = "games?date=2021:2022"
    response = requests.get(f'{base_url}{endpoint}')
    data = response.json()['data']

    # Extract the 'gameID' values from the response data
    game_ids = [item['gameID'] for item in data]
    # ids = pd.DataFrame(requests.get(f'{base_url}{endpoint}').json()['data'])['gameID'].values

    # game_events_proxy = GameEventsProxy()
    # all_games = []

    # # Add a progress bar with tqdm
    # for id in tqdm(ids, desc="Fetching game data", unit="game"):
    #     try:
    #         game_events = GameEvents(id, game_events_proxy)
    #         game_events.process_game_events()
    #         game_df = game_events.get_events_df(True, True, True)
    #         all_games.append(game_df)
    #     except AssertionError as e:
    #         print(f"Error with game ID {id}: {e}")
    #     except Exception as e:
    #         print(f"Error with game ID {id}: {e}")
    #         raise e

    # ALL_GAMES = pd.concat(all_games)
    return game_ids
