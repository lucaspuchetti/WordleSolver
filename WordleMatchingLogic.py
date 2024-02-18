
################### Objects ####################

# Letter
class Letter:
    def __init__(self, letter_tuple) -> None:
        self.pos, self.char= letter_tuple

    def __eq__(self, __value) -> bool:
        return self.pos == __value.pos and self.char == __value.char

    def __repr__(self) -> str:
        return f"{self.char}({self.pos})"

# Wordle words
class Word:
    def __init__(self, word:str):
        word = word.lower()
        self.word = word
        self.enumerated = list(map(lambda _: Letter(_), enumerate(word)))

class Guess(Word):
    pass

class Answer(Word):
    pass

# Match enums
class MatchLevel:
    def __init__(self, letter):
        self.letter = letter

    def match_level_is(self, other) -> bool:
        return type(self) == other

class FullMatch(MatchLevel):
    def __repr__(self):
        return f"{self.letter}: Match"

class PartialMatch(MatchLevel):
    def __repr__(self):
        return f"{self.letter}: PartialMatch"

class NoMatch(MatchLevel):
    def __repr__(self):
        return f"{self.letter}: NoMatch"

################### Logic ####################

def match_letter(letter, options):
    return list(filter(lambda x: x.char == letter.char, options))

def match_position(letter, options):
    return list(filter(lambda x: x.pos == letter.pos, options))

def remove_letter(letter, options):
    return list(filter(lambda x: x != letter, options))

def resolve_broken(letter, options):
    ## Doesnt work cause partialmatches can override full matches
    contained_letters = match_letter(letter, options)
    perfect_match = match_position(letter, contained_letters)
    decision = None
    new_options = options
    if perfect_match:
        decision = FullMatch(letter)
        new_options = remove_letter(letter, options)
    elif contained_letters == []:
        decision = NoMatch(letter)
        new_options = options
    else:
        decision = PartialMatch(letter)
        new_options = remove_letter(contained_letters[0] , options )
    return decision, new_options

def resolve_for_full(letter, options):
    contained_letters = match_letter(letter, options)
    perfect_match = match_position(letter, contained_letters)
    decision = None
    new_options = options
    if perfect_match:
        decision = FullMatch(letter)
        new_options = remove_letter(letter, options)
    else:
        decision = NoMatch(letter)
        new_options = options
    return decision, new_options

def resolve_for_partial(letter, options):
    contained_letters = match_letter(letter, options)
    decision = None
    new_options = options
    if contained_letters == []:
        decision = NoMatch(letter)
        new_options = options
    else:
        decision = PartialMatch(letter)
        new_options = remove_letter(contained_letters[0] , options )
    return decision, new_options

def resolve_word(guess, answer):
    current_options = answer.enumerated
    full_result = []
    for letter in guess.enumerated:
        decision, current_options = resolve_for_full(letter, current_options)
        full_result.append(decision)

    #current_options = [ _.letter for _ in full_result if _.match_level_is(NoMatch)]
    final_result = []
    for first_pass_letter in full_result:
        letter = first_pass_letter.letter
        if first_pass_letter.match_level_is(NoMatch):
            decision, current_options = resolve_for_partial(letter, current_options)
            final_result.append(decision)
        else:
            final_result.append(first_pass_letter)

    return final_result

################### TESTING ####################
guess = Guess("color")

perfMatch = Answer("color")

partialMatch = Answer("cowes")

partialDoubleLetterMatch = Answer("tilla")

noMatch = Answer("guess")

print("Guess: " , guess.word)

for test_word in [
    Answer("color"),
    Answer("roloc"),
    Answer("coooo"),
    Answer("cowes"),
    Answer("tilla"),
    Answer("tllia"),
    Answer("guess"),
    ]:
    print(test_word.word, ":", resolve_word(test_word, guess), "\n"*2)

