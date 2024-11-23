package com.unper.samper.util;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

public class EmptyStringAsNullDeserializer extends JsonDeserializer<String> {

    @Override
    public String deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {    
        String value = jsonParser.getText();
        if (value == null || value.isEmpty()) {
            return null;
        } else {
            return value;
        }
    }
}
