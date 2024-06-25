
#pragma once

#include <lmud/defs.h>


struct LMud_Utf8_Encoder
{
    LMud_Size  read;
    LMud_Size  write;
    char       buffer[5];
};

void LMud_Utf8_Encoder_Create(struct LMud_Utf8_Encoder* self, LMud_Rune rune);
void LMud_Utf8_Encoder_Destroy(struct LMud_Utf8_Encoder* self);

bool LMud_Utf8_Encoder_HasBytesToRead(struct LMud_Utf8_Encoder* self);
bool LMud_Utf8_Encoder_Read(struct LMud_Utf8_Encoder* self, char* byte);
bool LMud_Utf8_Encoder_ReadAll(struct LMud_Utf8_Encoder* self, char* buffer, LMud_Size size);

const char* LMud_Utf8_Encoder_AsString(struct LMud_Utf8_Encoder* self);


struct LMud_Utf8_Decoder
{
    LMud_Size  remaining;
    LMud_Rune  current_rune;
    bool       blocked;
};

void LMud_Utf8_Decoder_Create(struct LMud_Utf8_Decoder* self);
void LMud_Utf8_Decoder_Destroy(struct LMud_Utf8_Decoder* self);

bool LMud_Utf8_Decoder_IsComplete(struct LMud_Utf8_Decoder* self);
void LMud_Utf8_Decoder_Push(struct LMud_Utf8_Decoder* self, char byte);


LMud_Rune LMud_Rune_FromChar(char byte);
char      LMud_Rune_AsChar(LMud_Rune rune);

LMud_Rune LMud_Utf8_UpperCase(LMud_Rune rune);
LMud_Rune LMud_Utf8_LowerCase(LMud_Rune rune);
