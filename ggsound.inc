;Comment out these equates for features you do not wish to use.
FEATURE_DPCM = 1
FEATURE_ARPEGGIOS = 1

ifdef FEATURE_DPCM
DPCM_STATE_NOP = 0
DPCM_STATE_UPLOAD = 1
DPCM_STATE_UPLOAD_THEN_WAIT = 2
DPCM_STATE_WAIT = 3
endif

;Max number of music streams, sfx streams, and max total streams
;based on whether dpcm is enabled. soundeffect_one and soundeffect_two
;are always to be used when specifying sound effect priority.
ifdef FEATURE_DPCM
MAX_MUSIC_STREAMS = 5
soundeffect_one = 5
soundeffect_two = 6
else
MAX_MUSIC_STREAMS = 4
soundeffect_one = 4
soundeffect_two = 5
endif
MAX_SFX_STREAMS = 2
MAX_STREAMS = (MAX_MUSIC_STREAMS + MAX_SFX_STREAMS)

;****************************************************************
;The following are all opcodes. All opcodes in range 0-86 are
;interpreted as a note playback call. Everything 87 or above
;are interpreted as stream control opcodes.
;****************************************************************

A0  = 0
AS0 = 1
B0  = 2
C1  = 3
CS1 = 4
D1  = 5
DS1 = 6
E1  = 7
F1  = 8
FS1 = 9
G1  = 10
GS1 = 11
A1  = 12
AS1 = 13
B1  = 14
C2  = 15
CS2 = 16
D2  = 17
DS2 = 18
E2  = 19
F2  = 20
FS2 = 21
G2  = 22
GS2 = 23
A2  = 24
AS2 = 25
B2  = 26
C3  = 27
CS3 = 28
D3  = 29
DS3 = 30
E3  = 31
F3  = 32
FS3 = 33
G3  = 34
GS3 = 35
A3  = 36
AS3 = 37
B3  = 38
C4  = 39
CS4 = 40
D4  = 41
DS4 = 42
E4  = 43
F4  = 44
FS4 = 45
G4  = 46
GS4 = 47
A4  = 48
AS4 = 49
B4  = 50
C5  = 51
CS5 = 52
D5  = 53
DS5 = 54
E5  = 55
F5  = 56
FS5 = 57
G5  = 58
GS5 = 59
A5  = 60
AS5 = 61
B5  = 62
C6  = 63
CS6 = 64
D6  = 65
DS6 = 66
E6  = 67
F6  = 68
FS6 = 69
G6  = 70
GS6 = 71
A6  = 72
AS6 = 73
B6  = 74
C7  = 75
CS7 = 76
D7  = 77
DS7 = 78
E7  = 79
F7  = 80
FS7 = 81
G7  = 82
GS7 = 83
A7  = 84
AS7 = 85
B7  = 86

OPCODES_BASE = 87

;stream control opcodes

;set length opcodes for standard note lengths
SL1 = 0  + OPCODES_BASE
SL2 = 1  + OPCODES_BASE
SL3 = 2  + OPCODES_BASE
SL4 = 3  + OPCODES_BASE
SL5 = 4  + OPCODES_BASE
SL6 = 5  + OPCODES_BASE
SL7 = 6  + OPCODES_BASE
SL8 = 7  + OPCODES_BASE
SL9 = 8  + OPCODES_BASE
SLA = 9  + OPCODES_BASE
SLB = 10 + OPCODES_BASE
SLC = 11 + OPCODES_BASE
SLD = 12 + OPCODES_BASE
SLE = 13 + OPCODES_BASE
SLF = 14 + OPCODES_BASE
SL0 = 15 + OPCODES_BASE

;set length lo byte
SLL = 16+OPCODES_BASE

;set length hi byte
SLH = 17+OPCODES_BASE

;set volume envelope
STV = 18+OPCODES_BASE

;set pitch envelope
STP = 19+OPCODES_BASE

;set duty envelope
SDU = 20+OPCODES_BASE

;goto
GOT = 21+OPCODES_BASE

;call
CAL = 22+OPCODES_BASE

;return
RET = 23+OPCODES_BASE

;terminate
TRM = 24+OPCODES_BASE

ifdef FEATURE_ARPEGGIOS
;set arpeggio envelope
SAR = 25+OPCODES_BASE
endif

;opcodes read from volume and pitch envelopes. These values are also
;reserved by Famitracker, so they are safe to use.
ENV_STOP = %10000000 ;-128
ENV_LOOP = %01111111 ; 127

;a different set of opcodes for stop and loop for duty cycle envelopes.
;This is necessary since ENV_STOP can be intepreted as duty cycle = 2,
;preventing users from using that setting.
DUTY_ENV_STOP = %00111111
DUTY_ENV_LOOP = %00101010

;values for stream flags
STREAM_ACTIVE_SET         = %00000001
STREAM_ACTIVE_TEST        = %00000001
STREAM_ACTIVE_CLEAR       = %11111110

STREAM_SILENCE_SET        = %00000010
STREAM_SILENCE_TEST       = %00000010
STREAM_SILENCE_CLEAR      = %11111101

STREAM_PAUSE_SET          = %00000100
STREAM_PAUSE_TEST         = %00000100
STREAM_PAUSE_CLEAR        = %11111011

STREAM_PITCH_LOADED_SET   = %00001000
STREAM_PITCH_LOADED_TEST  = %00001000
STREAM_PITCH_LOADED_CLEAR = %11110111

;default tempo.
DEFAULT_TEMPO = 256 * 15

;These end up incrementing an offset when reading track headers
SOUND_REGION_NTSC = 0
SOUND_REGION_PAL  = 2

track_header_ntsc_tempo_lo            = 0
track_header_ntsc_tempo_hi            = 1
track_header_pal_tempo_lo             = 2
track_header_pal_tempo_hi             = 3
track_header_square1_stream_address   = 4
track_header_square2_stream_address   = 6
track_header_triangle_stream_address  = 8
track_header_noise_stream_address     = 10
ifdef FEATURE_DPCM
track_header_dpcm_stream_address      = 12
endif

macro advance_stream_read_address

    inc stream_read_address_lo,x
    bne +
    inc stream_read_address_hi,x
+

endm

;this macro updates the sound engine. It is intended to
;be used at the end of an nmi routine, after ppu synchronization.
macro soundengine_update

    lda sound_disable_update
    bne +

    jsr sound_update
    jsr sound_upload

+

endm
