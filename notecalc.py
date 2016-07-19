CPU = 1789773.0

def midi_note_to_nes_timer(frequency):
    timer = (CPU / (16 * frequency)) - 1
    return round(timer)
    
print(midi_note_to_nes_timer(40))
