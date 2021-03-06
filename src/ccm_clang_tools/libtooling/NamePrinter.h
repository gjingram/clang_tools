/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once
#include <clang/AST/DeclVisitor.h>
#include <clang/Basic/SourceManager.h>

#include "jsonlib/JSONWriter.h"
namespace ASTLib {

using namespace clang;
template <class JSONWriter>
class NamePrinter : public ConstDeclVisitor<NamePrinter<JSONWriter>> {
  typedef typename JSONWriter::ObjectScope ObjectScope;
  typedef typename JSONWriter::ArrayScope ArrayScope;
  typedef typename JSONWriter::TupleScope TupleScope;
  typedef typename JSONWriter::VariantScope VariantScope;

  const SourceManager &SM;
  JSONWriter &OF;

  PrintingPolicy getPrintingPolicy();
  void printTemplateArgList(llvm::raw_ostream &OS,
                            const ArrayRef<TemplateArgument> Args);

 public:
  NamePrinter(const SourceManager &SM, JSONWriter &OF) : SM(SM), OF(OF) {}

  // implementation is inspired by NamedDecl::printQualifiedName
  // but with better handling for anonymous structs,unions and namespaces
  auto getContexts(NamedDecl const &D);
  bool goodDeclName(const NamedDecl *D);
  void printDeclName(const NamedDecl &D);

  void VisitNamedDecl(const NamedDecl *D);
  void VisitNamespaceDecl(const NamespaceDecl *ND);
  void VisitTagDecl(const TagDecl *TD);
  void VisitFunctionDecl(const FunctionDecl *FD);
};

// 64 bits fnv-1a
const uint64_t FNV64_hash_start = 14695981039346656037ULL;
const uint64_t FNV64_prime = 1099511628211ULL;
uint64_t fnv64Hash(const char *s, int n) {
  uint64_t hash = FNV64_hash_start;
  for (int i = 0; i < n; ++i) {
    hash ^= s[i];
    hash *= FNV64_prime;
  }
  return hash;
}

uint64_t fnv64Hash(llvm::raw_svector_ostream &OS) {
  std::string s = OS.str();
  return fnv64Hash(s.data(), s.size());
}

const int templateLengthThreshold = 40;
template <class JSONWriter>
void NamePrinter<JSONWriter>::printTemplateArgList(
    llvm::raw_ostream &OS, const ArrayRef<TemplateArgument> Args) {
  SmallString<64> Buf;
  llvm::raw_svector_ostream tmpOS(Buf);
  clang::printTemplateArgumentList(tmpOS, Args, getPrintingPolicy());
  if (tmpOS.str().size() > templateLengthThreshold) {
    OS << "<";
    // OS.write_hex(fnv64Hash(tmpOS));
    OS << tmpOS.str();
    OS << ">";
  } else {
    OS << tmpOS.str();
  }
}

template <class JSONWriter>
auto NamePrinter<JSONWriter>::getContexts(NamedDecl const &D) {

    const DeclContext *ctx = D.getDeclContext();
    SmallVector<const NamedDecl *, 8> contexts;
    contexts.push_back(&D);

    if (ctx->isFunctionOrMethod() && !isa<TagDecl>(D)) {
        ctx = nullptr;
    }

    while (ctx && isa<NamedDecl>(ctx)) {
        bool should_print_ctx = true;
        if (const NamespaceDecl *ND = dyn_cast<NamespaceDecl>(ctx)) {
            if (ND->isInline()) {
                should_print_ctx = false;
            }
        }
        if (should_print_ctx) {
            contexts.push_back(cast<NamedDecl>(ctx));
        }
        ctx = ctx->getParent();
    }

    return contexts;

}


template <class JSONWriter>
bool NamePrinter<JSONWriter>::goodDeclName(NamedDecl const *D) {
    auto contexts = getContexts(*D);
    for (auto &ctx : contexts) {
        std::string ctx_name = ctx->getNameAsString();
        if (!ctx_name.rfind("__", 0) || !ctx_name.rfind("typename __", 0)) {
            return false;
        }
    }
    return true;
}

template <class JSONWriter>
void NamePrinter<JSONWriter>::printDeclName(const NamedDecl &D) {

  const DeclContext *Ctx = D.getDeclContext();
  SmallVector<const NamedDecl *, 8> Contexts;
  Contexts.push_back(&D);

  // Don't dump full qualifier for variables declared
  // inside a function/method/block
  // For structs defined inside functions, dump fully qualified name
  if (Ctx->isFunctionOrMethod() && !isa<TagDecl>(&D)) {
    Ctx = nullptr;
  }

  while (Ctx && isa<NamedDecl>(Ctx)) {
    bool shouldPrintCtx = true;
    // skip inline namespaces when printing qualified name
    if (const NamespaceDecl *ND = dyn_cast<NamespaceDecl>(Ctx)) {
      if (ND->isInline()) {
        shouldPrintCtx = false;
      }
    }
    if (shouldPrintCtx) {
      Contexts.push_back(cast<NamedDecl>(Ctx));
    }
    Ctx = Ctx->getParent();
  }

  ArrayScope aScope(OF, Contexts.size());
  // dump list in reverse
  for (const Decl *Ctx : Contexts) {
    ConstDeclVisitor<NamePrinter<JSONWriter>>::Visit(Ctx);
  }

  return;
}

template <class JSONWriter>
void NamePrinter<JSONWriter>::VisitNamedDecl(const NamedDecl *D) {
  OF.emitString(D->getNameAsString());
}

template <class JSONWriter>
void NamePrinter<JSONWriter>::VisitNamespaceDecl(const NamespaceDecl *ND) {
  if (ND->isAnonymousNamespace()) {
    PresumedLoc PLoc = SM.getPresumedLoc(ND->getLocation());
    std::string file = "invalid_loc";
    if (PLoc.isValid()) {
      file = PLoc.getFilename();
    }
    OF.emitString("anonymous_namespace_" + file);
  } else {
    // for non-anonymous namespaces, fallback to normal behavior
    VisitNamedDecl(ND);
  }
}

template <class JSONWriter>
void NamePrinter<JSONWriter>::VisitTagDecl(const TagDecl *D) {
  // heavily inspired by clang's TypePrinter::printTag() function
  SmallString<64> Buf;
  llvm::raw_svector_ostream StrOS(Buf);
  if (const IdentifierInfo *II = D->getIdentifier()) {
    StrOS << II->getName();
  } else if (TypedefNameDecl *Typedef = D->getTypedefNameForAnonDecl()) {
    StrOS << Typedef->getIdentifier()->getName();
  } else {
    if (isa<CXXRecordDecl>(D) && cast<CXXRecordDecl>(D)->isLambda()) {
      StrOS << "lambda";
    } else {
      StrOS << "anonymous_" << D->getKindName();
    }
    PresumedLoc PLoc = SM.getPresumedLoc(D->getLocation());
    if (PLoc.isValid()) {
      StrOS << "_" << PLoc.getFilename() << ':' << PLoc.getLine() << ':'
            << PLoc.getColumn();
    }
  }
  if (const ClassTemplateSpecializationDecl *Spec =
          dyn_cast<ClassTemplateSpecializationDecl>(D)) {
    ArrayRef<TemplateArgument> Args;
    const TemplateArgumentList &TemplateArgs = Spec->getTemplateArgs();
    Args = TemplateArgs.asArray();
    printTemplateArgList(StrOS, Args);
  }
  OF.emitString(StrOS.str());
}

template <class JSONWriter>
void NamePrinter<JSONWriter>::VisitFunctionDecl(const FunctionDecl *FD) {
  std::string template_str = "";
  // add instantiated template arguments for readability
  if (const TemplateArgumentList *TemplateArgs =
          FD->getTemplateSpecializationArgs()) {
    SmallString<64> Buf;
    llvm::raw_svector_ostream StrOS(Buf);
    printTemplateArgList(StrOS, TemplateArgs->asArray());
    template_str = StrOS.str();
  }
  OF.emitString(FD->getNameAsString() + template_str);
}

template <class JSONWriter>
PrintingPolicy NamePrinter<JSONWriter>::getPrintingPolicy() {
  // configure what to print
  LangOptions LO;
  PrintingPolicy Policy(LO);
  // print tag types
  Policy.IncludeTagDefinition = false;
  // print fully qualified names - this is needed by PrintTemplateArgumentList
  Policy.SuppressScope = false;
  // don't print inline and anonymous namespaces
  Policy.SuppressUnwrittenScope = true;
  // print locations of anonymous tags
  Policy.AnonymousTagLocations = true;
  // don't add 'struct' inside a name regardless of language
  Policy.SuppressTagKeyword = true;

  return Policy;
}

} // end of namespace ASTLib
