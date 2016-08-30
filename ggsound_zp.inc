sound_region: .dsb 1
sound_disable_update: .dsb 1
sound_local_byte_0: .dsb 1
sound_local_byte_1: .dsb 1
sound_local_byte_2: .dsb 1

sound_local_word_0: .dsb 2
sound_local_word_1: .dsb 2
sound_local_word_2: .dsb 2

sound_param_byte_0: .dsb 1
sound_param_byte_1: .dsb 1
sound_param_byte_2: .dsb 1

sound_param_word_0: .dsb 2
sound_param_word_1: .dsb 2
sound_param_word_2: .dsb 2
sound_param_word_3: .dsb 2

base_address_volume_envelopes: .dsb 2
ifdef FEATURE_ARPEGGIOS
base_address_arpeggio_envelopes: .dsb 2
endif
base_address_pitch_envelopes:  .dsb 2
base_address_duty_envelopes:   .dsb 2
base_address_note_table_lo: .dsb 2
base_address_note_table_hi: .dsb 2
ifdef FEATURE_DPCM
base_address_dpcm_sample_table: .dsb 2
base_address_dpcm_note_to_sample_index: .dsb 2
base_address_dpcm_note_to_sample_length: .dsb 2
base_address_dpcm_note_to_loop_pitch_index: .dsb 2
endif

stream_byte: .dsb 1

apu_data_ready: .dsb 1
apu_square_1_old: .dsb 1
apu_square_2_old: .dsb 1
ifdef FEATURE_DPCM
apu_dpcm_state: .dsb 1
endif

song_list_address: .dsb 2
sfx_list_address: .dsb 2
song_address: .dsb 2
apu_register_sets: .dsb 20
