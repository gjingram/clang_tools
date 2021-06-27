/*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "../JSONWriter.h"

typedef JSONWriter::JsonWriter<std::ostream> JsonWriter;
typedef JsonWriter::ObjectScope ObjectScope;
typedef JsonWriter::ArrayScope ArrayScope;
typedef JsonWriter::VariantScope VariantScope;
typedef JsonWriter::TupleScope TupleScope;

int main(int argc, char **argv) {
  const struct JSONWriter::JSONWriterOptions jsonWriterOptions = {
      .prettifyJson = true
  };
  {
    JsonWriter OF(std::cout, jsonWriterOptions);
    TupleScope Scope(OF, 2);
    OF.emitSimpleVariant("zero");
    {
      VariantScope Scope(OF, "succ");
      {
        VariantScope Scope(OF, "pred");
        OF.emitSimpleVariant("zero");
      }
    }
  }
  return 0;
}
