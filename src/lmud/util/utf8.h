
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

bool LMud_Utf8_Decoder_GetRune(struct LMud_Utf8_Decoder* self, LMud_Rune* rune);


LMud_Rune    LMud_Rune_FromChar(char byte);
LMud_Rune    LMud_Rune_FromInteger(LMud_Integer value);
char         LMud_Rune_AsChar(LMud_Rune rune);
LMud_Integer LMud_Rune_AsInteger(LMud_Rune rune);

LMud_Rune LMud_Utf8_UpperCase(LMud_Rune rune);
LMud_Rune LMud_Utf8_LowerCase(LMud_Rune rune);

bool        LMud_Rune_ByName(const char* name, LMud_Rune* rune);
const char* LMud_Rune_Name(LMud_Rune rune);
