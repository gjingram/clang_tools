/**
 * Copyright (c) 2014, Facebook, Inc.  * Copyright (c) 2003-2014 University of Illinois at Urbana-Champaign.
 * All rights reserved.
 *
 * This file is distributed under the University of Illinois Open Source
 * License.
 * See LLVM-LICENSE for details.
 *
 */

/**
 * Utility class to export an AST of clang into Json and Yojson (and ultimately
 * Biniou)
 * while conforming to the inlined ATD specifications.
 *
 * /!\
 * '//@atd' comments are meant to be extracted and processed to generate ATD
 * specifications for the Json dumper.
 * Do not modify ATD comments without modifying the Json emission accordingly
 * (and conversely).
 * See ATD_GUIDELINES.md for more guidelines on how to write and test ATD
 * annotations.
 *
 * This file was obtained by modifying the file ASTdumper.cpp from the
 * LLVM/clang project.
 * The general layout should be maintained to make future merging easier.
 */

#ifndef ASTEXPORTER_H
#define ASTEXPORTER_H

#include <memory>
#include <cstring>

#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Attr.h>
#include <clang/AST/AttrVisitor.h>
#include <clang/AST/CommentVisitor.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/DeclLookups.h>
#include <clang/AST/DeclObjC.h>
#include <clang/AST/DeclVisitor.h>
#include <clang/AST/Mangle.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/StmtVisitor.h>
#include <clang/AST/TypeVisitor.h>
#include <clang/Basic/Module.h>
#include <clang/Basic/SourceManager.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendDiagnostic.h>
#include <clang/Frontend/FrontendPluginRegistry.h>
#include <clang/Lex/Lexer.h>

#include <llvm/Support/raw_ostream.h>

#include "AttrParameterVectorStream.h"
#include "NamePrinter.h"
#include "SimplePluginASTAction.h"
#include <iostream>
#include <string>

//===----------------------------------------------------------------------===//
// ASTExporter Visitor
//===----------------------------------------------------------------------===//

namespace ASTLib {

struct ASTExporterOptions : ASTPluginLib::PluginASTOptionsBase {
  bool withPointers = true;
  bool dumpComments = true;
  bool useMacroExpansionLocation = true;
  JSONWriter::JSONWriterOptions jsonWriterOptions = {
      .prettifyJson = false
  };

  void loadValuesFromEnvAndMap(
      const ASTPluginLib::PluginASTOptionsBase::argmap_t &map) {
    ASTPluginLib::PluginASTOptionsBase::loadValuesFromEnvAndMap(map);
    loadBool(map, "AST_WITH_POINTERS", withPointers);
    loadBool(map, "PRETTIFY_JSON", jsonWriterOptions.prettifyJson);
  }
};

using namespace clang;
using namespace clang::comments;

template <class Impl>
struct TupleSizeBase {
  // Decls

#define DECL(DERIVED, BASE)                              \
  int DERIVED##DeclTupleSize() {                         \
    return static_cast<Impl *>(this)->BASE##TupleSize(); \
  }
#define ABSTRACT_DECL(DECL) DECL
#include <clang/AST/DeclNodes.inc>

  int tupleSizeOfDeclKind(const Decl::Kind kind) {
    switch (kind) {
#define DECL(DERIVED, BASE) \
  case Decl::DERIVED:       \
    return static_cast<Impl *>(this)->DERIVED##DeclTupleSize();
#define ABSTRACT_DECL(DECL)
#include <clang/AST/DeclNodes.inc>
    }
    llvm_unreachable("Decl that isn't part of DeclNodes.inc!");
  }

  // Stmts

#define STMT(CLASS, PARENT)                                \
  int CLASS##TupleSize() {                                 \
    return static_cast<Impl *>(this)->PARENT##TupleSize(); \
  }
#define ABSTRACT_STMT(STMT) STMT
#include <clang/AST/StmtNodes.inc>

  int tupleSizeOfStmtClass(const Stmt::StmtClass stmtClass) {
    switch (stmtClass) {
#define STMT(CLASS, PARENT) \
  case Stmt::CLASS##Class:  \
    return static_cast<Impl *>(this)->CLASS##TupleSize();
#define ABSTRACT_STMT(STMT)
#include <clang/AST/StmtNodes.inc>
    case Stmt::NoStmtClass:
      break;
    }
    llvm_unreachable("Stmt that isn't part of StmtNodes.inc!");
  }

  // Comments

#define COMMENT(CLASS, PARENT)                             \
  int CLASS##TupleSize() {                                 \
    return static_cast<Impl *>(this)->PARENT##TupleSize(); \
  }
#define ABSTRACT_COMMENT(COMMENT) COMMENT
#include <clang/AST/CommentNodes.inc>

  int tupleSizeOfCommentKind(const Comment::CommentKind kind) {
    switch (kind) {
#define COMMENT(CLASS, PARENT) \
  case Comment::CLASS##Kind:   \
    return static_cast<Impl *>(this)->CLASS##TupleSize();
#define ABSTRACT_COMMENT(COMMENT)
#include <clang/AST/CommentNodes.inc>
    case Comment::NoCommentKind:
      break;
    }
    llvm_unreachable("Comment that isn't part of CommentNodes.inc!");
  }

  // Types

#define TYPE(DERIVED, BASE)                              \
  int DERIVED##TypeTupleSize() {                         \
    return static_cast<Impl *>(this)->BASE##TupleSize(); \
  }
#define ABSTRACT_TYPE(DERIVED, BASE) TYPE(DERIVED, BASE)
#include <clang/AST/TypeNodes.inc>

  int tupleSizeOfTypeClass(const Type::TypeClass typeClass) {
    switch (typeClass) {
#define TYPE(DERIVED, BASE) \
  case Type::DERIVED:       \
    return static_cast<Impl *>(this)->DERIVED##TypeTupleSize();
#define ABSTRACT_TYPE(DERIVED, BASE)
#include <clang/AST/TypeNodes.inc>
    }
    llvm_unreachable("Type that isn't part of TypeNodes.inc!");
  }

  // Attributes

#define ATTR(NAME) \
  int NAME##AttrTupleSize() { return 1; }
#include <clang/Basic/AttrList.inc>

  int tupleSizeOfAttrKind(const attr::Kind attrKind) {
    switch (attrKind) {
#define ATTR(NAME)       \
  case attr::Kind::NAME: \
    return static_cast<Impl *>(this)->NAME##AttrTupleSize();
#include <clang/Basic/AttrList.inc>
    }
    llvm_unreachable("Attr that isn't part of AttrList.inc!");
  }
};

typedef JSONWriter::JsonWriter<raw_ostream> JsonWriter;

template <class ATDWriter = JsonWriter>
class ASTExporter : public ConstDeclVisitor<ASTExporter<ATDWriter>>,
                    public ConstStmtVisitor<ASTExporter<ATDWriter>>,
                    public ConstCommentVisitor<ASTExporter<ATDWriter>>,
                    public TypeVisitor<ASTExporter<ATDWriter>>,
                    public ConstAttrVisitor<ASTExporter<ATDWriter>>,
                    public TupleSizeBase<ASTExporter<ATDWriter>> {

  using ObjectScope = typename ATDWriter::ObjectScope;
  using ArrayScope = typename ATDWriter::ArrayScope;
  using TupleScope = typename ATDWriter::TupleScope;
  using VariantScope = typename ATDWriter::VariantScope;

  ATDWriter OF;
  ASTContext &Context;

  const ASTExporterOptions &Options;

  std::unique_ptr<MangleContext> Mangler;

  // Encoding of NULL pointers into suitable empty nodes
  // This is a hack but using option types in children lists would make the Json
  // terribly verbose.
  // Also these useless nodes could have occurred in the original AST anyway :)
  //
  // Note: We are not using std::unique_ptr because 'delete' appears to be
  // protected (at least on Stmt).
  const Stmt *const NullPtrStmt;
  const Decl *const NullPtrDecl;
  const Comment *const NullPtrComment;

  // Keep track of the last location we print out so that we can
  // print out deltas from then on out.
  const char *LastLocFilename;
  unsigned LastLocLine;

  // The \c FullComment parent of the comment being dumped.
  const FullComment *FC;
  std::string comment_text;

  NamePrinter<ATDWriter> NamePrint;

 public:
  ASTExporter(raw_ostream &OS,
              ASTContext &Context,
              const ASTExporterOptions &Opts)
      : OF(OS, Opts.jsonWriterOptions),
        Context(Context),
        Options(Opts),
        Mangler(
            ItaniumMangleContext::create(Context, Context.getDiagnostics())),
        NullPtrStmt(new (Context) NullStmt(SourceLocation())),
        NullPtrDecl(EmptyDecl::Create(
            Context, Context.getTranslationUnitDecl(), SourceLocation())),
        NullPtrComment(new (Context) Comment(
            Comment::NoCommentKind, SourceLocation(), SourceLocation())),
        LastLocFilename(""),
        LastLocLine(~0U),
        FC(0),
        NamePrint(Context.getSourceManager(), OF) {}

  void dumpSourceFile(SourceLocation);
  void dumpDecl(const Decl *D);
  void dumpStmt(const Stmt *S);
  void dumpFullComment(const FullComment *C);
  void dumpType(const Type *T);
  void dumpPointerToType(const Type *T);
  void dumpQualTypeNoQuals(const QualType &qt);
  void dumpClassLambdaCapture(const LambdaCapture *C);
  void dumpVersionTuple(const VersionTuple &VT);

  // Utilities
  void dumpPointer(const void *Ptr);
  void dumpSourceRange(SourceRange R);
  void dumpSourceLocation(SourceLocation Loc);
  void dumpQualType(const QualType &qt);
  void dumpTypeOld(const Type *T);
  void dumpDeclRef(const Decl &Node);
  bool hasNodes(const DeclContext *DC);
  void dumpLookups(const DeclContext &DC);
  void dumpSelector(const Selector sel);
  void dumpName(const NamedDecl &decl);
  void dumpInputKind(const InputKind kind);
  void dumpIntegerTypeWidths(const TargetInfo &info);

  bool alwaysEmitParent(const Decl *D);

  void emitAPInt(bool isSigned, const llvm::APInt &value);

  // C++ Utilities
  void dumpAccessSpecifier(AccessSpecifier AS);
  void dumpCXXCtorInitializer(const CXXCtorInitializer &Init);
  void dumpDeclarationName(const DeclarationName &Name);
  void dumpNestedNameSpecifierLoc(NestedNameSpecifierLoc NNS);
  void dumpTemplateTypeParmDecl(const TemplateDecl *T,
                                const TemplateTypeParmDecl *TTP);
  void dumpNonTypeTemplateParmDecl(const TemplateDecl *T,
                                   const NonTypeTemplateParmDecl *NTTP);
  void dumpTemplateTemplateParmDecl(const TemplateDecl *T,
                                    const TemplateTemplateParmDecl *TTMP);
  void dumpTemplateArgument(const TemplateArgument &Arg);
  void dumpTemplateSpecialization(const TemplateDecl *D,
                                  const TemplateArgumentList &Args);
  /*
  void dumpTemplateArgumentListInfo(const TemplateArgumentListInfo &TALI);
  void dumpTemplateArgumentLoc(const TemplateArgumentLoc &A);
  void dumpTemplateArgumentList(const TemplateArgumentList &TAL);
  void dumpTemplateArgument(const TemplateArgument &A,
                                SourceRange R = SourceRange());
  */
  void dumpCXXBaseSpecifier(const CXXBaseSpecifier &Base);
  void dumpTemplateParameters(const TemplateDecl *D,
                              const TemplateParameterList *TPL);

#define DECLARE_VISITOR(NAME) \
  int NAME##TupleSize();      \
  void Visit##NAME(const NAME *D);
#define DECLARE_LOWERCASE_VISITOR(NAME) \
  int NAME##TupleSize();                \
  void visit##NAME(const NAME *D);

#define NO_IMPL(NAME) \
  int NAME##TupleSize() { return -1; } \
  void visit##NAME(const NAME *D) { return; }

  // Decls
  DECLARE_VISITOR(Decl)
  //NO_IMPL(BlockDecl)
  DECLARE_VISITOR(DeclContext)
  DECLARE_VISITOR(CapturedDecl)
  DECLARE_VISITOR(LinkageSpecDecl)
  DECLARE_VISITOR(NamespaceDecl)
  //DECLARE_VISITOR(ObjCContainerDecl)
  DECLARE_VISITOR(TagDecl)
  DECLARE_VISITOR(TypeDecl)
  DECLARE_VISITOR(TranslationUnitDecl)
  DECLARE_VISITOR(NamedDecl)
  DECLARE_VISITOR(ValueDecl)
  DECLARE_VISITOR(TypedefDecl)
  DECLARE_VISITOR(EnumDecl)
  DECLARE_VISITOR(RecordDecl)
  DECLARE_VISITOR(EnumConstantDecl)
  DECLARE_VISITOR(IndirectFieldDecl)
  DECLARE_VISITOR(FunctionDecl)
  DECLARE_VISITOR(FieldDecl)
  DECLARE_VISITOR(VarDecl)
  // no use for these yet, ignore them
  // DECLARE_VISITOR(FileScopeAsmDecl)
  DECLARE_VISITOR(ImportDecl)

  // C++ Decls
  DECLARE_VISITOR(UsingDirectiveDecl)
  DECLARE_VISITOR(NamespaceAliasDecl)
  DECLARE_VISITOR(CXXRecordDecl)
  DECLARE_VISITOR(ClassTemplateSpecializationDecl)
  DECLARE_VISITOR(CXXMethodDecl)
  DECLARE_VISITOR(CXXConstructorDecl)
  DECLARE_VISITOR(ClassTemplateDecl)
  DECLARE_VISITOR(FunctionTemplateDecl)
  DECLARE_VISITOR(FriendDecl)
  DECLARE_VISITOR(TypeAliasDecl)
  DECLARE_VISITOR(TypeAliasTemplateDecl)

  void VisitClassTemplatePartialSpecializationDecl(
      const ClassTemplatePartialSpecializationDecl *D);

  //NO_IMPL(ObjCIvarDecl)
  //NO_IMPL(ObjCMethodDecl)
  //NO_IMPL(ObjCCategoryDecl)
  //NO_IMPL(ObjCProtocolDecl)
  //NO_IMPL(ObjCInterfaceDecl)
  //NO_IMPL(ObjCImplementationDecl)
  //NO_IMPL(ObjCCompatibleAliasDecl)
  //NO_IMPL(ObjCPropertyDecl)
  //NO_IMPL(ObjCPropertyImplDecl)

  // Stmts.
  DECLARE_VISITOR(Stmt)
  //DECLARE_VISITOR(AttributedStmt)
  //NO_IMPL(CXXCatchStmt)
  DECLARE_VISITOR(DeclStmt)
  //NO_IMPL(GotoStmt)
  //NO_IMPL(IfStmt)
  //NO_IMPL(LabelStmt)
  //NO_IMPL(SwitchStmt)

  // Exprs
  DECLARE_VISITOR(Expr)
  //NO_IMPL(CastExpr)
  //NO_IMPL(ExplicitCastExpr)
  DECLARE_VISITOR(DeclRefExpr)
  //DECLARE_VISITOR(PredefinedExpr)
  DECLARE_VISITOR(CharacterLiteral)
  DECLARE_VISITOR(IntegerLiteral)
  DECLARE_VISITOR(FixedPointLiteral)
  DECLARE_VISITOR(FloatingLiteral)
  DECLARE_VISITOR(StringLiteral)
  DECLARE_VISITOR(MemberExpr)
  DECLARE_VISITOR(OverloadExpr)
  DECLARE_VISITOR(CXXDefaultArgExpr)
  DECLARE_VISITOR(CXXDefaultInitExpr)

  //NO_IMPL(UnaryOperator)
  //NO_IMPL(UnaryExprOrTypeTraitExpr)

  // C++
  //DECLARE_VISITOR(LambdaExpr)

  // Comments.
  const char *getCommandName(unsigned CommandID);
  void dumpComment(const Comment *C);

  // Inline comments.
  DECLARE_LOWERCASE_VISITOR(Comment)
  // DECLARE_LOWERCASE_VISITOR(TextComment)
  //    void visitInlineCommandComment(const InlineCommandComment *C);
  //    void visitHTMLStartTagComment(const HTMLStartTagComment *C);
  //    void visitHTMLEndTagComment(const HTMLEndTagComment *C);
  //
  //    // Block comments.
  //    void visitBlockCommandComment(const BlockCommandComment *C);
  //    void visitParamCommandComment(const ParamCommandComment *C);
  //    void visitTParamCommandComment(const TParamCommandComment *C);
  //    void visitVerbatimBlockComment(const VerbatimBlockComment *C);
  //    void visitVerbatimBlockLineComment(const VerbatimBlockLineComment *C);
  //    void visitVerbatimLineComment(const VerbatimLineComment *C);

  // Types - no template type handling yet
  int TypeWithChildInfoTupleSize();
  DECLARE_VISITOR(Type)
  DECLARE_VISITOR(AdjustedType)
  DECLARE_VISITOR(ArrayType)
  DECLARE_VISITOR(ConstantArrayType)
  //  DECLARE_VISITOR(DependentSizedArrayType)
  //  DECLARE_VISITOR(IncompleteArrayType)
  DECLARE_VISITOR(VariableArrayType)
  DECLARE_VISITOR(AtomicType)
  DECLARE_VISITOR(AttributedType) // getEquivalentType() + getAttrKind -> string
  //  DECLARE_VISITOR(AutoType)
  DECLARE_VISITOR(BlockPointerType)
  DECLARE_VISITOR(BuiltinType)
  //  DECLARE_VISITOR(ComplexType)
  DECLARE_VISITOR(DecltypeType)
  //  DECLARE_VISITOR(DependentSizedExtVectorType)
  DECLARE_VISITOR(FunctionType)
  //  DECLARE_VISITOR(FunctionNoProtoType)
  DECLARE_VISITOR(FunctionProtoType)
  DECLARE_VISITOR(MemberPointerType)
  DECLARE_VISITOR(ParenType)
  DECLARE_VISITOR(PointerType)
  DECLARE_VISITOR(ReferenceType)
  DECLARE_VISITOR(TagType)
  DECLARE_VISITOR(TypedefType)
  DECLARE_VISITOR(TemplateTypeParmType)
  DECLARE_VISITOR(SubstTemplateTypeParmType)
  DECLARE_VISITOR(TemplateSpecializationType)
  DECLARE_VISITOR(InjectedClassNameType)
  DECLARE_VISITOR(DependentNameType)


  void dumpAttrKind(attr::Kind Kind);
  void dumpAttr(const Attr *A);
  DECLARE_VISITOR(Attr)
  DECLARE_VISITOR(AnnotateAttr)
  DECLARE_VISITOR(AvailabilityAttr)
  DECLARE_VISITOR(SentinelAttr)
  DECLARE_VISITOR(VisibilityAttr)

  void dumpTypeAttr(AttributedType::Kind kind);

  private:
  void writePointer(bool withPointers, const void *Ptr);

  /* #define TYPE(CLASS, PARENT) DECLARE_VISITOR(CLASS##Type) */
  /* #define ABSTRACT_TYPE(CLASS, PARENT) */
  /* #include <clang/AST/TypeNodes.def> */
};

//===----------------------------------------------------------------------===//
//  Utilities
//===----------------------------------------------------------------------===//

bool hasMeaningfulTypeInfo(const Type *T) {
  // clang goes into an infinite loop trying to compute the TypeInfo of
  // dependent types, and a width of 0 if the type doesn't have a constant size
  return T && !T->isIncompleteType() && !T->isDependentType() &&
         T->isConstantSizeType();
}

std::unordered_map<const void *, int> pointerMap;
int pointerCounter = 1;

template <class ATDWriter>
void ASTExporter<ATDWriter>::writePointer(bool withPointers, const void *Ptr) {
  ObjectScope oScope(OF, 1);

  OF.emitTag("pointer");
  if (!Ptr) {
    OF.emitInteger(0);
    return;
  }
  if (pointerMap.find(Ptr) == pointerMap.end()) {
    pointerMap[Ptr] = pointerCounter++;
  }
  OF.emitInteger(pointerMap[Ptr]);

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpPointer(const void *Ptr) {
  writePointer(Options.withPointers, Ptr);
  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpSourceFile(SourceLocation Loc) {
  const SourceManager &SM = Context.getSourceManager();
  SourceLocation ExpLoc =
    Options.useMacroExpansionLocation ? SM.getExpansionLoc(Loc) : Loc;
  SourceLocation SpellingLoc = SM.getSpellingLoc(ExpLoc);

  PresumedLoc PLoc = SM.getPresumedLoc(SpellingLoc);

  OF.emitTag("file");
  if (PLoc.isInvalid()) {
    OF.emitString("Unknown");
  } else {
    OF.emitString(Options.normalizeSourcePath(PLoc.getFilename()));
  }

  return;

} 
  
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpSourceLocation(SourceLocation Loc) {
  const SourceManager &SM = Context.getSourceManager();
  SourceLocation ExpLoc =
      Options.useMacroExpansionLocation ? SM.getExpansionLoc(Loc) : Loc;
  SourceLocation SpellingLoc = SM.getSpellingLoc(ExpLoc);

  // The general format we print out is filename:line:col, but we drop pieces
  // that haven't changed since the last loc printed.
  PresumedLoc PLoc = SM.getPresumedLoc(SpellingLoc);

  ObjectScope Scope(OF, 2);
  if (PLoc.isInvalid()) {
    
    OF.emitTag("line");
    OF.emitString("Unknown");

    OF.emitTag("column");
    OF.emitString("Unknown");

    return;

  }

  OF.emitTag("line");
  OF.emitInteger(PLoc.getLine());
  OF.emitTag("column");
  OF.emitInteger(PLoc.getColumn());

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpSourceRange(SourceRange R) {
  
  ObjectScope Scope(OF, 3);

  OF.emitTag("file");
  dumpSourceFile(R.getBegin());

  OF.emitTag("begin");
  dumpSourceLocation(R.getBegin());

  OF.emitTag("end");
  dumpSourceLocation(R.getEnd());
  
  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpQualType(const QualType &qt) {
  
  clang::Qualifiers Quals =
      qt.isNull() ? clang::Qualifiers() : qt.getQualifiers();
  bool isConst = Quals.hasConst();
  bool isRestrict = Quals.hasRestrict();
  bool isVolatile = Quals.hasVolatile();


  ObjectScope oScope(OF, 5);

  OF.emitTag("type_ptr");
  dumpQualTypeNoQuals(qt);

  OF.emitTag("type_name");
  PrintingPolicy pp(Context.getLangOpts());
  OF.emitString(qt.getAsString(pp));

  OF.emitTag("is_const");
  OF.emitBoolean(isConst);

  OF.emitTag("is_restrict");
  OF.emitBoolean(isRestrict);

  OF.emitTag("is_volatile");
  OF.emitBoolean(isVolatile);

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpName(const NamedDecl &Decl) {

  ObjectScope oScope(OF, 2);
  OF.emitTag("name");

  std::string name = Decl.getNameAsString();
  if (name.length() == 0) {
    const FieldDecl *FD = dyn_cast<FieldDecl>(&Decl);
    if (FD) {
      name = "__anon_field_" + std::to_string(FD->getFieldIndex());
    }
  }
  OF.emitString(name);

  OF.emitTag("qual_name");
  NamePrint.printDeclName(Decl);

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpDeclRef(const Decl &D) {

  const NamedDecl *ND = dyn_cast<NamedDecl>(&D);
  const ValueDecl *VD = dyn_cast<ValueDecl>(&D);
  bool IsHidden = ND && ND->isHidden();
  ObjectScope Scope(OF, 5);

  OF.emitTag("kind");
  OF.emitString(D.getDeclKindName());

  OF.emitTag("decl_pointer");
  dumpPointer(&D);

  OF.emitTag("name");
  if (ND) {
    dumpName(*ND);
  } else {
    ObjectScope oScope(OF, 0);
  }
   
  OF.emitTag("is_hidden");
  OF.emitBoolean(IsHidden); 

  OF.emitTag("qual_type");
  if (VD) {
    dumpQualType(VD->getType());
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::DeclContextTupleSize() {
  return 2;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitDeclContext(const DeclContext *DC) {

  ObjectScope oScope(OF, 4);
  if (!DC) {
    
    OF.emitTag("c_linkage");
    OF.emitString("Unknown");

    OF.emitTag("has_external_lexical_storage");
    OF.emitString("Unknown");

    OF.emitTag("has_external_visible_storage");
    OF.emitString("Unknown");

    OF.emitTag("declarations");
    {
      ArrayScope aScope(OF, 0);
    }

    return;

  }

  OF.emitTag("c_linkage");
  OF.emitBoolean(DC->isExternCContext());

  OF.emitTag("has_external_lexical_storage");
  OF.emitBoolean(DC->hasExternalLexicalStorage());

  OF.emitTag("has_external_visible_storage");
  OF.emitBoolean(DC->hasExternalVisibleStorage());

  std::vector<Decl*> declsToDump;
  for (auto I : DC->decls()) {
    declsToDump.push_back(I);
  }

  if (isa<TranslationUnitDecl>(DC) &&
    Context.getObjCInstanceType().getTypePtrOrNull()) {
      declsToDump.push_back(Context.getObjCInstanceTypeDecl());
  }

  OF.emitTag("declarations");

  ArrayScope aScope(OF, declsToDump.size());
  for (auto I : declsToDump) {
    dumpDecl(I);
  } 

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpLookups(const DeclContext &DC) {
  ObjectScope Scope(OF, 4); // not covered by tests

  OF.emitTag("decl_ref");
  dumpDeclRef(cast<Decl>(DC));

  const DeclContext *Primary = DC.getPrimaryContext();
  OF.emitTag("primary_context_pointer");
  dumpPointer(cast<Decl>(Primary));

  OF.emitTag("lookups");
  {
    ArrayScope Scope(OF);
    DeclContext::all_lookups_iterator I = Primary->noload_lookups_begin(),
                                      E = Primary->noload_lookups_end();
    while (I != E) {
      DeclarationName Name = I.getLookupName();
      DeclContextLookupResult R = *I++;

      ObjectScope Scope(OF, 2); // not covered by tests
      OF.emitTag("decl_name");
      OF.emitString(Name.getAsString());

      OF.emitTag("decl_refs");
      {
        ArrayScope Scope(OF);
        for (DeclContextLookupResult::iterator RI = R.begin(), RE = R.end();
             RI != RE;
             ++RI) {
          dumpDeclRef(**RI);
        }
      }
    }
  }

  bool HasUndeserializedLookups = Primary->hasExternalVisibleStorage();
  OF.emitTag("has_undeserialized_decls");
  OF.emitBoolean(HasUndeserializedLookups);

  return;

}

//===----------------------------------------------------------------------===//
//  C++ Utilities
//===----------------------------------------------------------------------===//

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpAccessSpecifier(AccessSpecifier AS) {
  OF.emitTag("access_spec");
  switch (AS) {
    case AS_public:
    OF.emitString("public");
    break;
  case AS_protected:
    OF.emitString("protected");
    break;
  case AS_private:
    OF.emitString("private");
    break;
  case AS_none:
    OF.emitString("None");
    break;
  }
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpCXXCtorInitializer(
    const CXXCtorInitializer &Init) {
  
  const Expr *E = Init.getInit();
  ObjectScope Scope(OF, 4);

  const FieldDecl *FD = Init.getAnyMember();
  bool isBaseClass = false;

  OF.emitTag("kind");
  if (FD) {
    
    OF.emitString("member");

    OF.emitTag("declaration");
    dumpDeclRef(*FD);

    OF.emitTag("qualified_type");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("virtual_base");
    OF.emitBoolean(false);

  } else if (Init.isDelegatingInitializer()) {

    OF.emitString("delegated");

    OF.emitTag("declaration");
    OF.emitString("None");

    OF.emitTag("qualified_type");
    dumpQualTypeNoQuals(Init.getTypeSourceInfo()->getType());

    OF.emitTag("virtual_base");
    OF.emitBoolean(false);

  } else {

    OF.emitString("base_class");
    isBaseClass = true;

    OF.emitTag("declaration");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("qualified_type");
    dumpQualTypeNoQuals(Init.getTypeSourceInfo()->getType());

    OF.emitTag("virtual_base");
    OF.emitBoolean(Init.isBaseVirtual());

  }

  OF.emitTag("source_range");
  dumpSourceRange(Init.getSourceRange());

  OF.emitTag("init_expr");
  if (E) {
    dumpStmt(E);
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpDeclarationName(const DeclarationName &Name) {
  ObjectScope Scope(OF, 2); // not covered by tests

  OF.emitTag("kind");
  switch (Name.getNameKind()) {
  case DeclarationName::Identifier:
    OF.emitString("Identifier");
    break;
  case DeclarationName::ObjCZeroArgSelector:
    OF.emitString("ObjCZeroArgSelector");
    break;
  case DeclarationName::ObjCOneArgSelector:
    OF.emitString("ObjCOneArgSelector");
    break;
  case DeclarationName::ObjCMultiArgSelector:
    OF.emitString("ObjCMultiArgSelector");
    break;
  case DeclarationName::CXXConstructorName:
    OF.emitString("CXXConstructorName");
    break;
  case DeclarationName::CXXDestructorName:
    OF.emitString("CXXDestructorName");
    break;
  case DeclarationName::CXXConversionFunctionName:
    OF.emitString("CXXConversionFunctionName");
    break;
  case DeclarationName::CXXOperatorName:
    OF.emitString("CXXOperatorName");
    break;
  case DeclarationName::CXXLiteralOperatorName:
    OF.emitString("CXXLiteralOperatorName");
    break;
  case DeclarationName::CXXUsingDirective:
    OF.emitString("CXXUsingDirective");
    break;
  case DeclarationName::CXXDeductionGuideName:
    OF.emitString("CXXDeductionGuideName");
    break;
  }
  OF.emitTag("name");
  OF.emitString(Name.getAsString());
 
  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpNestedNameSpecifierLoc(
    NestedNameSpecifierLoc NNS) {

  SmallVector<NestedNameSpecifierLoc, 8> NestedNames;
  while (NNS) {
    NestedNames.push_back(NNS);
    NNS = NNS.getPrefix();
  }

  ArrayScope Scope(OF, NestedNames.size());
  while (!NestedNames.empty()) {
    NNS = NestedNames.pop_back_val();
    NestedNameSpecifier::SpecifierKind Kind =
        NNS.getNestedNameSpecifier()->getKind();
    ObjectScope Scope(OF, 2);

    OF.emitTag("kind");
    switch (Kind) {
    case NestedNameSpecifier::Identifier:
      OF.emitString("Identifier");
      OF.emitTag("ref");
      {
        ObjectScope oScope(OF, 0);
      }
      break;
    case NestedNameSpecifier::Namespace:
      OF.emitString("Namespace");
      OF.emitTag("ref");
      dumpDeclRef(*NNS.getNestedNameSpecifier()->getAsNamespace());
      break;
    case NestedNameSpecifier::NamespaceAlias:
      OF.emitString("NamespaceAlias");
      OF.emitTag("ref");
      dumpDeclRef(*NNS.getNestedNameSpecifier()->getAsNamespaceAlias());
      break;
    case NestedNameSpecifier::TypeSpec:
      OF.emitString("TypeSpec");
      OF.emitTag("ref");
      {
        ObjectScope oScope(OF, 0);
      }
      break;
    case NestedNameSpecifier::TypeSpecWithTemplate:
      OF.emitString("TypeSpecWithTemplate");
      OF.emitTag("ref");
      {
        ObjectScope oScope(OF, 0);
      }
      break;
    case NestedNameSpecifier::Global:
      OF.emitString("Global");
      OF.emitTag("ref");
      {
        ObjectScope oScope(OF, 0);
      }
      break;
    case NestedNameSpecifier::Super:
      OF.emitString("Super");
      OF.emitTag("ref");
      {
        ObjectScope oScope(OF, 0);
      }
      break;
    }
  }
  return;
}

template <class ATDWriter>
bool ASTExporter<ATDWriter>::alwaysEmitParent(const Decl *D) {
  if (isa<ObjCMethodDecl>(D) || isa<CXXMethodDecl>(D) || isa<FieldDecl>(D) ||
      isa<ObjCIvarDecl>(D) || isa<BlockDecl>(D) ||
      isa<ObjCInterfaceDecl>(D) || isa<ObjCImplementationDecl>(D) ||
      isa<ObjCCategoryDecl>(D) || isa<ObjCCategoryImplDecl>(D) ||
      isa<ObjCPropertyDecl>(D) || isa<RecordDecl>(D)
      || isa<ObjCProtocolDecl>(D) ) {
    return true;
  }
  return false;
}

//===----------------------------------------------------------------------===//
//  Decl dumping methods.
//===----------------------------------------------------------------------===//

//#define DECL(DERIVED, BASE)
//#define ABSTRACT_DECL(DECL) DECL
//#include <clang/AST/DeclNodes.inc>

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpDecl(const Decl *D) {
  OF.emitTag("kind");
  OF.emitString(std::string(D->getDeclKindName()) + "Decl");
  if (!D) {
    // We use a fixed EmptyDecl node to represent null pointers
    D = NullPtrDecl;
  }
  {
    OF.emitTag("decl_content");
    ArrayScope aScope(OF, ASTExporter::tupleSizeOfDeclKind(D->getKind()));
    ConstDeclVisitor<ASTExporter<ATDWriter>>::Visit(D);
  }
  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::DeclTupleSize() {
  return 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitDecl(const Decl *D) {
  {

    bool ShouldEmitParentPointer =
        alwaysEmitParent(D) ||
        D->getLexicalDeclContext() != D->getDeclContext();

    Module *M = D->getImportedOwningModule();
    if (!M) {
      M = D->getLocalOwningModule();
    }

    const NamedDecl *ND = dyn_cast<NamedDecl>(D);
    bool IsNDHidden = ND && ND->isHidden();
    bool IsDImplicit = D->isImplicit();
    bool IsDUsed = D->isUsed();
    bool IsDReferenced = D->isThisDeclarationReferenced();
    bool IsDInvalid = D->isInvalidDecl();
    bool HasAttributes = D->hasAttrs();
    const FullComment *Comment =
        Options.dumpComments
            ? D->getASTContext().getLocalCommentForDeclUncached(D)
            : nullptr;
    AccessSpecifier Access = D->getAccess();
    bool HasAccess = Access != AccessSpecifier::AS_none;

    ObjectScope Scope(OF, 10);

    OF.emitTag("pointer");
    dumpPointer(D);

    OF.emitTag("parent_pointer");
    if (ShouldEmitParentPointer) {
      dumpPointer(cast<Decl>(D->getDeclContext()));
    } else {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("source_range");
    dumpSourceRange(D->getSourceRange());

    OF.emitTag("owning_module");
    if (M) {
      OF.emitString(M->getFullModuleName());
    } else {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("is_hidden");
    OF.emitBoolean(IsNDHidden);

    OF.emitTag("is_implicit");
    OF.emitBoolean(IsDImplicit);

    OF.emitTag("is_used");
    OF.emitBoolean(IsDUsed);

    OF.emitTag("is_this_declaration_referenced");
    OF.emitBoolean(IsDReferenced);

    OF.emitTag("is_invalid_decl");
    OF.emitBoolean(IsDInvalid);

    OF.emitTag("attributes");
    if (HasAttributes) {
      ArrayScope ArrayAttr(OF, D->getAttrs().size());
      for (auto I : D->getAttrs()) {
        dumpAttr(I);
      }
    } else {
      ArrayScope aScope(OF, 0);
    }

    OF.emitTag("full_comment");
    if (Comment) {
      dumpFullComment(Comment);
    } else {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("access_specifier");
    if (HasAccess) {
      dumpAccessSpecifier(Access);
    } else {
      ObjectScope oScope(OF, 0);
    }

  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CapturedDeclTupleSize() {
  return DeclTupleSize() + DeclContextTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCapturedDecl(const CapturedDecl *D) {
  VisitDecl(D);
  VisitDeclContext(D);
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::LinkageSpecDeclTupleSize() {
  return DeclTupleSize() + DeclContextTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitLinkageSpecDecl(const LinkageSpecDecl *D) {
  VisitDecl(D);
  VisitDeclContext(D);
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::NamespaceDeclTupleSize() {
  return NamedDeclTupleSize() + DeclContextTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitNamespaceDecl(const NamespaceDecl *D) {
  VisitNamedDecl(D);
  VisitDeclContext(D);

  bool IsInline = D->isInline();
  bool IsOriginalNamespace = D->isOriginalNamespace();
  ObjectScope Scope(OF, 2);

  OF.emitTag("is_inline");
  OF.emitBoolean(IsInline);

  OF.emitTag("original_namespace");
  if (!IsOriginalNamespace) {
    dumpDeclRef(*D->getOriginalNamespace());
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TagDeclTupleSize() {
  return TypeDeclTupleSize() + DeclContextTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTagDecl(const TagDecl *D) {
  VisitTypeDecl(D);
  VisitDeclContext(D);

  ObjectScope(OF, 1);

  OF.emitTag("kind");
  switch (D->getTagKind()) {
  case TagTypeKind::TTK_Struct:
    OF.emitString("TTK_Struct");
    break;
  case TagTypeKind::TTK_Interface:
    OF.emitString("TTK_Interface");
    break;
  case TagTypeKind::TTK_Union:
    OF.emitString("TTK_Union");
    break;
  case TagTypeKind::TTK_Class:
    OF.emitString("TTK_Class");
    break;
  case TagTypeKind::TTK_Enum:
    OF.emitString("TTK_Enum");
    break;
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypeDeclTupleSize() {
  return NamedDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTypeDecl(const TypeDecl *D) {
  ObjectScope oScope(OF, 2);

  OF.emitTag("named_decl");
  VisitNamedDecl(D);
  const Type *T = D->getTypeForDecl();

  OF.emitTag("type_pointer");
  dumpPointerToType(T);

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ValueDeclTupleSize() {
  return NamedDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitValueDecl(const ValueDecl *D) {
  ObjectScope oScope(OF, 2);

  OF.emitTag("named_decl");
  VisitNamedDecl(D);

  OF.emitTag("qualified_type");
  dumpQualType(D->getType());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TranslationUnitDeclTupleSize() {
  return DeclTupleSize() + DeclContextTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpInputKind(InputKind kind) {
  // Despite here we deal only with the language field of InputKind, there are
  // new info in InputKind that can still be used, e.g. whether the source is
  // preprocessed (PP), or precompiled.

  ObjectScope oScope(OF, 1);

  OF.emitTag("kind");

  switch (kind.getLanguage()) {
  case Language::Unknown:
    OF.emitString("Language_None");
    break;
  case Language::Asm:
    OF.emitString("Language_Asm");
    break;
  case Language::C:
    OF.emitString("Language_C");
    break;
  case Language::CXX:
    OF.emitString("Language_CXX");
    break;
  case Language::ObjC:
    OF.emitString("Language_ObjC");
    break;
  case Language::ObjCXX:
    OF.emitString("Language_ObjCXX");
    break;
  case Language::OpenCL:
    OF.emitString("Language_OpenCL");
    break;
  case Language::CUDA:
    OF.emitString("Language_CUDA");
    break;
  case Language::RenderScript:
    OF.emitString("Language_RenderScript");
    break;
  case Language::LLVM_IR:
    OF.emitString("Language_LLVM_IR");
    break;
  case Language::HIP:
    OF.emitString("Language_HIP");
    break;
  }

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpIntegerTypeWidths(const TargetInfo &info) {
  ObjectScope Scope(OF, 5);

  OF.emitTag("char_type");
  OF.emitInteger(info.getCharWidth());
  OF.emitTag("short_type");
  OF.emitInteger(info.getShortWidth());
  OF.emitTag("int_type");
  OF.emitInteger(info.getIntWidth());
  OF.emitTag("long_type");
  OF.emitInteger(info.getLongWidth());
  OF.emitTag("longlong_type");
  OF.emitInteger(info.getLongLongWidth());

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTranslationUnitDecl(
    const TranslationUnitDecl *D) {
  VisitDecl(D);
  VisitDeclContext(D);
  ObjectScope Scope(OF, 4);

  OF.emitTag("input_path");
  OF.emitString(
      Options.normalizeSourcePath(Options.inputFile.getFile().str().c_str()));

  OF.emitTag("input_kind");
  dumpInputKind(Options.inputFile.getKind());

  OF.emitTag("integer_type_widths");
  dumpIntegerTypeWidths(Context.getTargetInfo());

  OF.emitTag("types");
  const auto &types = Context.getTypes();
  ArrayScope aScope(OF, types.size() + 1); // + 1 for nullptr
  for (const Type *type : types) {
    dumpType(type);
  }

  dumpType(nullptr);

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::NamedDeclTupleSize() {
  return DeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitNamedDecl(const NamedDecl *D) {
  VisitDecl(D);
  dumpName(*D);

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypedefDeclTupleSize() {
  return ASTExporter::TypedefNameDeclTupleSize() + 2;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTypedefDecl(const TypedefDecl *D) {
  ASTExporter<ATDWriter>::VisitTypedefNameDecl(D);

  bool IsModulePrivate = D->isModulePrivate();
  ObjectScope Scope(OF, 2);

  OF.emitTag("underlying_type");
  dumpQualType(D->getUnderlyingType());

  OF.emitTag("is_module_private");
  OF.emitBoolean(IsModulePrivate);

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::EnumDeclTupleSize() {
  return TagDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitEnumDecl(const EnumDecl *D) {
  VisitTagDecl(D);

  bool IsScoped = D->isScoped();
  bool IsModulePrivate = D->isModulePrivate();
  ObjectScope Scope(OF, 2); // not covered by tests

  OF.emitTag("scope");

  if (IsScoped) {
    if (D->isScopedUsingClassTag())
      OF.emitString("Class");
    else
      OF.emitString("Struct");
  } else {
      OF.emitString("None");
  }

  OF.emitTag("is_module_private");
  OF.emitBoolean(IsModulePrivate);

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::RecordDeclTupleSize() {
  return TagDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitRecordDecl(const RecordDecl *D) {
  VisitTagDecl(D);

  bool IsModulePrivate = D->isModulePrivate();
  bool IsCompleteDefinition = D->isCompleteDefinition();
  bool IsDependentType = D->isDependentType();
  ObjectScope Scope(
      OF, 4)
	  ;
  OF.emitTag("definition_ptr");
  dumpPointer(D->getDefinition());

  OF.emitTag("is_module_private");
  OF.emitBoolean(IsModulePrivate);

  OF.emitTag("is_complete_definition");
  OF.emitBoolean(IsCompleteDefinition);

  OF.emitTag("is_dependent_type");
  OF.emitBoolean(IsDependentType);

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::EnumConstantDeclTupleSize() {
  return ValueDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitEnumConstantDecl(const EnumConstantDecl *D) {
  VisitValueDecl(D);

  const Expr *Init = D->getInitExpr();
  ObjectScope Scope(OF, 1); // not covered by tests

  OF.emitTag("init_expr");
  if (Init) {
    dumpStmt(Init);
  } else {
    ObjectScope oScope(OF, 0);
  }
  
  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::IndirectFieldDeclTupleSize() {
  return ValueDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitIndirectFieldDecl(
    const IndirectFieldDecl *D) {
  VisitValueDecl(D);

  OF.emitTag("decl_refs");
  ArrayScope Scope(
      OF,
      std::distance(D->chain_begin(), D->chain_end())); // not covered by tests
  for (auto I : D->chain()) {
    dumpDeclRef(*I);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FunctionDeclTupleSize() {
  return ASTExporter::DeclaratorDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFunctionDecl(const FunctionDecl *D) {
  ASTExporter<ATDWriter>::VisitDeclaratorDecl(D);
  // We purposedly do not call VisitDeclContext(D).

  bool ShouldMangleName = Mangler->shouldMangleDeclName(D);
  bool IsInlineSpecified = D->isInlineSpecified();
  bool IsModulePrivate = D->isModulePrivate();
  bool IsPure = D->isPure();
  bool IsDeletedAsWritten = D->isDeletedAsWritten();
  bool IsCpp = Mangler->getASTContext().getLangOpts().CPlusPlus;
  bool IsVariadic = D->isVariadic();
  bool IsStatic = false; // static functions
  if (D->getStorageClass() == SC_Static) {
    IsStatic = true;
  }
  auto IsNoReturn = D->isNoReturn();
  bool HasParameters = !D->param_empty();
  const FunctionDecl *DeclWithBody = D;
  // FunctionDecl::hasBody() will set DeclWithBody pointer to decl that
  // has body. If there is no body in all decls of that function,
  // then we need to set DeclWithBody to nullptr manually
  if (!D->hasBody(DeclWithBody)) {
    DeclWithBody = nullptr;
  }
  bool HasDeclarationBody = D->doesThisDeclarationHaveABody();
  FunctionTemplateDecl *TemplateDecl = D->getPrimaryTemplate();
  ObjectScope Scope(OF, 13);

  OF.emitTag("mangled_name");
  if (ShouldMangleName) {
    OF.emitTag("mangled_name");
    SmallString<64> Buf;
    llvm::raw_svector_ostream StrOS(Buf);
    if (const auto *CD = dyn_cast<CXXConstructorDecl>(D)) {
      Mangler->mangleCXXCtor(CD, Ctor_Complete, StrOS);
    } else if (const auto *DD = dyn_cast<CXXDestructorDecl>(D)) {
      Mangler->mangleCXXDtor(DD, Dtor_Deleting, StrOS);
    } else {
      Mangler->mangleName(D, StrOS);
    }
    // mangled names can get ridiculously long, so hash them to a fixed size
    OF.emitString(std::to_string(fnv64Hash(StrOS)));
  } else {
    OF.emitString("None");
  }

  OF.emitTag("is_cpp");
  OF.emitBoolean(IsCpp);

  OF.emitTag("is_inline");
  OF.emitBoolean(IsInlineSpecified);

  OF.emitTag("is_module_private");
  OF.emitBoolean(IsModulePrivate);

  OF.emitTag("is_pure");
  OF.emitBoolean(IsPure);

  OF.emitTag("is_deleted_as_written");
  OF.emitBoolean(IsDeletedAsWritten);

  OF.emitTag("is_no+_return");
  OF.emitBoolean(IsNoReturn);

  OF.emitTag("is_variadic");
  OF.emitBoolean(IsVariadic);

  OF.emitTag("is_static");
  OF.emitBoolean(IsStatic);

  OF.emitTag("parameters");
  if (HasParameters) {
    FunctionDecl::param_const_iterator I = D->param_begin(), E = D->param_end();
    if (I != E) {
      ArrayScope Scope(OF, std::distance(I, E));
      for (; I != E; ++I) {
        dumpDecl(*I);
      }
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("decl_ptr_with_body");
  if (DeclWithBody) {
    dumpPointer(DeclWithBody);
  } else {
    ObjectScope oScope(OF, 0);
  }

  OF.emitTag("body");
  if (HasDeclarationBody) {
    const Stmt *Body = D->getBody();
    if (Body) {
      dumpStmt(Body);
    }
  } else {
    ObjectScope oScope(OF, 0);
  }

  OF.emitTag("template_specialization");
  if (TemplateDecl) {
    dumpTemplateSpecialization(TemplateDecl,
                               *D->getTemplateSpecializationArgs());
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FieldDeclTupleSize() {
  return ASTExporter::DeclaratorDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFieldDecl(const FieldDecl *D) {
  ASTExporter<ATDWriter>::VisitDeclaratorDecl(D);

  bool IsMutable = D->isMutable();
  bool IsModulePrivate = D->isModulePrivate();
  bool HasBitWidth = D->isBitField() && D->getBitWidth();
  Expr *Init = D->getInClassInitializer();
  ObjectScope Scope(OF, 4); // not covered by tests

  OF.emitTag("is_mutable");
  OF.emitBoolean(IsMutable);

  OF.emitTag("is_module_private");
  OF.emitBoolean(IsModulePrivate);

  OF.emitTag("bit_width_expr");
  if (HasBitWidth) {
    dumpStmt(D->getBitWidth());
  } else {
    ObjectScope oScope(OF, 0);
  }

  OF.emitTag("init_expr");
  if (Init) {
    dumpStmt(Init);
  } else {
    ObjectScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::VarDeclTupleSize() {
  return ASTExporter::DeclaratorDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitVarDecl(const VarDecl *D) {
  ASTExporter<ATDWriter>::VisitDeclaratorDecl(D);

  bool IsGlobal = D->hasGlobalStorage(); // including static function variables
  bool IsExtern = D->hasExternalStorage();
  bool IsStatic = false; // static variables
  if (D->getStorageClass() == SC_Static) {
    IsStatic = true;
  }
  bool IsStaticLocal = D->isStaticLocal(); // static function variables
  bool IsStaticDataMember = D->isStaticDataMember();
  bool IsConstExpr = D->isConstexpr();
  bool IsInitICE = D->isInitKnownICE() && D->isInitICE();
  bool HasInit = D->hasInit();
  const ParmVarDecl *ParmDecl = dyn_cast<ParmVarDecl>(D);
  Expr *def_expr = nullptr;
  if (ParmDecl) {
    def_expr = const_cast<Expr*>(ParmDecl->getDefaultArg());
  }
  bool HasDefault = (bool) def_expr;
  bool HasParmIndex = (bool)ParmDecl;
  bool isInitExprCXX11ConstantExpr = false;
  ObjectScope Scope(OF, 10);

  OF.emitTag("is_global");
  OF.emitBoolean(IsGlobal);

  OF.emitTag("is_extern");
  OF.emitBoolean(IsExtern);

  OF.emitTag("is_static");
  OF.emitBoolean(IsStatic);

  OF.emitTag("is_static_local");
  OF.emitBoolean(IsStaticLocal);

  OF.emitTag("is_static_data_member");
  OF.emitBoolean(IsStaticDataMember);

  OF.emitTag("is_const_expr");
  OF.emitBoolean(IsConstExpr);

  OF.emitTag("is_init_ice");
  OF.emitBoolean(IsInitICE);

  OF.emitTag("has_default");
  OF.emitBoolean(HasDefault);

  OF.emitTag("init_expr");
  if (HasInit) {
    dumpStmt(D->getInit());
  } else {
    ObjectScope oScope(OF, 0);
  }

  OF.emitTag("is_init_expr_cxx11_constant");
  OF.emitBoolean(isInitExprCXX11ConstantExpr);

  OF.emitTag("parm_index_in_function");
  if (HasParmIndex) {
    OF.emitInteger(ParmDecl->getFunctionScopeIndex());
  } else {
    OF.emitString("None");
  }
  
  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ImportDeclTupleSize() {
  return DeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitImportDecl(const ImportDecl *D) {
  VisitDecl(D);

  OF.emitTag("module_name");
  OF.emitString(D->getImportedModule()->getFullModuleName());

  return;

}

//===----------------------------------------------------------------------===//
// C++ Declarations
//===----------------------------------------------------------------------===//

template <class ATDWriter>
int ASTExporter<ATDWriter>::UsingDirectiveDeclTupleSize() {
  return NamedDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitUsingDirectiveDecl(
    const UsingDirectiveDecl *D) {
  VisitNamedDecl(D);

  bool HasNominatedNamespace = D->getNominatedNamespace();
  ObjectScope Scope(OF, 4);

  OF.emitTag("using_location");
  dumpSourceLocation(D->getUsingLoc());

  OF.emitTag("namespace_key_location");
  dumpSourceLocation(D->getNamespaceKeyLocation());

  OF.emitTag("nested_name_specifier_locs");
  dumpNestedNameSpecifierLoc(D->getQualifierLoc());

  OF.emitTag("nominated_namespace");
  if (HasNominatedNamespace) {
    dumpDeclRef(*D->getNominatedNamespace());
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::NamespaceAliasDeclTupleSize() {
  return NamedDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitNamespaceAliasDecl(
    const NamespaceAliasDecl *D) {
  VisitNamedDecl(D);

  ObjectScope Scope(OF, 4);

  OF.emitTag("namespace_loc");
  dumpSourceLocation(D->getNamespaceLoc());

  OF.emitTag("target_name_loc");
  dumpSourceLocation(D->getTargetNameLoc());

  OF.emitTag("nested_name_specifier_locs");
  dumpNestedNameSpecifierLoc(D->getQualifierLoc());

  OF.emitTag("namespace");
  dumpDeclRef(*D->getNamespace());

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpClassLambdaCapture(const LambdaCapture *C) {

  LambdaCaptureKind CK = C->getCaptureKind();
  bool CapturesThis = C->capturesThis();
  bool CapturesVariable = C->capturesVariable();
  bool CapturesVLAType = C->capturesVLAType();
  VarDecl *decl = C->capturesVariable() ? C->getCapturedVar() : nullptr;
  bool IsInitCapture = decl && decl->isInitCapture();
  bool IsImplicit = C->isImplicit();
  SourceRange source_range = C->getLocation();
  bool IsPackExpansion = C->isPackExpansion();
  ObjectScope Scope(OF, 9);

  OF.emitTag("capture_kind");
  switch (CK) {
  case LCK_This:
    OF.emitString("LCK_This");
    break;
  case LCK_ByCopy:
    OF.emitString("LCK_ByCopy");
    break;
  case LCK_ByRef:
    OF.emitString("LCK_ByRef");
    break;
  case LCK_VLAType:
    OF.emitString("LCK_VLAType");
    break;
  case LCK_StarThis:
    OF.emitString("LCK_StarThis");
    break;
  };

  OF.emitTag("captures_this");
  OF.emitBoolean(CapturesThis);

  OF.emitTag("captures_variable");
  OF.emitBoolean(CapturesVariable);

  OF.emitTag("captures_VLAType");
  OF.emitBoolean(CapturesVLAType);

  OF.emitTag("init_captured_vardecl");
  if (decl) {
    if (IsInitCapture) {
      dumpDecl(decl);
    } else {
      ObjectScope oScope(OF, 0);
    }
  }

  OF.emitTag("captured_var");
  if (decl) {
    dumpDeclRef(*decl);
  } else {
    ObjectScope oScope(OF, 0);
  }

  OF.emitTag("is_implicit");
  OF.emitBoolean(IsImplicit);  

  OF.emitTag("location");
  dumpSourceRange(source_range);

  OF.emitTag("is_pack_expansion");
  OF.emitBoolean(IsPackExpansion);

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CXXRecordDeclTupleSize() {
  return RecordDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCXXRecordDecl(const CXXRecordDecl *D) {
  VisitRecordDecl(D);

  if (!D->isCompleteDefinition()) {
    ObjectScope Scope(OF, 0);
    return;
  }

  SmallVector<CXXBaseSpecifier, 2> nonVBases;
  SmallVector<CXXBaseSpecifier, 2> vBases;
  for (const auto base : D->bases()) {
    if (base.isVirtual()) {
      vBases.push_back(base);
    } else {
      nonVBases.push_back(base);
    }
  }

  bool HasVBases = vBases.size() > 0;
  bool HasNonVBases = nonVBases.size() > 0;
  unsigned numTransitiveVBases = D->getNumVBases();
  bool HasTransitiveVBases = numTransitiveVBases > 0;
  bool IsPOD = D->isPOD();
  const CXXDestructorDecl *DestructorDecl = D->getDestructor();
  const CXXMethodDecl *LambdaCallOperator = D->getLambdaCallOperator();

  auto I = D->captures_begin(), E = D->captures_end();
  ObjectScope Scope(OF, 9);

  OF.emitTag("is_polymorphic");
  OF.emitBoolean(D->isPolymorphic());

  OF.emitTag("is_abstract");
  OF.emitBoolean(D->isAbstract());

  OF.emitTag("bases");
  if (HasNonVBases || HasVBases || HasTransitiveVBases) {

    ArrayScope aScope(OF, D->bases().end() - D->bases().begin());
    for (const auto base : D->bases()) {
      
      ObjectScope base_scope(OF, 4);

      OF.emitTag("type");
      dumpQualTypeNoQuals(base.getType());

      OF.emitTag("access_spec");
      dumpAccessSpecifier(base.getAccessSpecifier());

      OF.emitTag("is_virtual");
      OF.emitBoolean(base.isVirtual());

      OF.emitTag("is_transitive");
      OF.emitBoolean(false);

    }

    for (const auto base : D->vbases()) {

      ObjectScope base_scope(OF, 4);

      OF.emitTag("type");
      dumpQualTypeNoQuals(base.getType());

      OF.emitTag("access_spec");
      dumpAccessSpecifier(base.getAccessSpecifier());

      OF.emitTag("is_virtual");
      OF.emitBoolean(base.isVirtual());

      OF.emitTag("is_transitive");
      OF.emitBoolean(true);

    }

  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("is_pod");
  OF.emitBoolean(IsPOD);

  OF.emitTag("destructor");
  if (DestructorDecl) {
    dumpDeclRef(*DestructorDecl);
  } else {
    ObjectScope oScope(OF, 0);
  }

  OF.emitTag("lambda_call_operator");
  if (LambdaCallOperator) {
    dumpDeclRef(*LambdaCallOperator);
  } else {
    ObjectScope oScope(OF, 0);
  }

  OF.emitTag("lambda_captures");
  if (I != E) {
    ArrayScope Scope(OF, std::distance(I, E));
    for (; I != E; ++I) {
      dumpClassLambdaCapture(I);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpTemplateArgument(const TemplateArgument &Arg) {

  OF.emitTag("kind");
  switch (Arg.getKind()) {
  case TemplateArgument::Null:
    OF.emitString("Null");

    OF.emitTag("type");
    {
      ObjectScope oScope(OF,0);
    }

    OF.emitTag("pointer");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    {
      ArrayScope aScope(OF, 0);
    }

    break;
  case TemplateArgument::Type: {
    OF.emitString("Type");

    OF.emitTag("type");
    dumpQualType(Arg.getAsType());

    OF.emitTag("pointer");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    {
      ArrayScope aScope(OF, 0);
    }

    break;

  }
  case TemplateArgument::Declaration: {
    OF.emitString("Declaration");

    OF.emitTag("type");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("pointer");
    dumpPointer(Arg.getAsDecl());

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    {
      ArrayScope aScope(OF, 0);
    }

    break;

  }
  case TemplateArgument::NullPtr:

    OF.emitString("NullPtr");

    OF.emitTag("type");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("pointer");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    {
      ArrayScope aScope(OF, 0);
    }

    break;
  case TemplateArgument::Integral: {
    VariantScope Scope(OF, "Integral");
    OF.emitString(Arg.getAsIntegral().toString(10));
    break;
  }
  case TemplateArgument::Template: {

    OF.emitString("Template");

    OF.emitTag("type");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("pointer");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    {
      ArrayScope aScope(OF, 0);
    }

    break;
  }
  case TemplateArgument::TemplateExpansion: {
    OF.emitString("TemplateExpansion");

    OF.emitTag("type");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("pointer");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    {
      ArrayScope aScope(OF, 0);
    }

    break;
  }
  case TemplateArgument::Expression: {
    OF.emitString("Expression");

    OF.emitTag("type");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("pointer");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    {
      ArrayScope aScope(OF, 0);
    }
 
    break;
  }
  case TemplateArgument::Pack: {
    OF.emitString("Pack");

    OF.emitTag("type");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("pointer");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    ArrayScope aScope(OF, Arg.pack_size());
    for (TemplateArgument::pack_iterator I = Arg.pack_begin(),
                                         E = Arg.pack_end();
         I != E;
         ++I) {
      dumpTemplateArgument(*I);
    }
    break;
  }
  }
  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpTemplateSpecialization(
    const TemplateDecl *D, const TemplateArgumentList &Args) {
  bool HasTemplateArgs = Args.size() > 0;
  ObjectScope oScope(OF, 1 + HasTemplateArgs);
  OF.emitTag("template_decl");
  dumpPointer(D);

  OF.emitTag("specialization_args");
  if (HasTemplateArgs) {
    OF.emitTag("specialization_args");
    ArrayScope aScope(OF, Args.size());
    for (size_t i = 0; i < Args.size(); i++) {
      dumpTemplateArgument(Args[i]);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ClassTemplateSpecializationDeclTupleSize() {
  return CXXRecordDeclTupleSize() + 2;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitClassTemplateSpecializationDecl(
    const ClassTemplateSpecializationDecl *D) {
  VisitCXXRecordDecl(D);

  bool ShouldMangleName = Mangler->shouldMangleDeclName(D);

  OF.emitTag("mangled_name");
  if (ShouldMangleName) {
    SmallString<64> Buf;
    llvm::raw_svector_ostream StrOS(Buf);
    Mangler->mangleName(D, StrOS);
    OF.emitString(std::to_string(fnv64Hash(StrOS)));
  } else {
    OF.emitString("");
  }

  dumpTemplateSpecialization(D->getSpecializedTemplate(), D->getTemplateArgs());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CXXMethodDeclTupleSize() {
  return FunctionDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCXXMethodDecl(const CXXMethodDecl *D) {
  VisitFunctionDecl(D);
  bool IsVirtual = D->isVirtual();
  bool IsStatic = D->isStatic();
  const CXXConstructorDecl *C = dyn_cast<CXXConstructorDecl>(D);
  bool HasCtorInitializers = C && C->init_begin() != C->init_end();
  bool IsConstexpr = D->isConstexpr();
  auto OB = D->begin_overridden_methods();
  auto OE = D->end_overridden_methods();
  ObjectScope Scope(OF, 5);

  OF.emitTag("is_virtual");
  OF.emitBoolean(IsVirtual);

  OF.emitTag("is_static");
  OF.emitBoolean(IsStatic);

  OF.emitTag("is_constexpr");
  OF.emitBoolean(IsConstexpr);

  OF.emitTag("cxx_ctor_initializers");
  if (HasCtorInitializers) {
    ArrayScope Scope(OF, std::distance(C->init_begin(), C->init_end()));
    for (auto I : C->inits()) {
      dumpCXXCtorInitializer(*I);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("overriden_methods");
  if (OB != OE) {
    ArrayScope Scope(OF, std::distance(OB, OE));
    for (; OB != OE; ++OB) {
      dumpDeclRef(**OB);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CXXConstructorDeclTupleSize() {
  return CXXMethodDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCXXConstructorDecl(const CXXConstructorDecl *D) {
    VisitCXXMethodDecl(D);    
    ObjectScope Scope(OF, 4);

    OF.emitTag("is_default");
    OF.emitBoolean(D->isDefaultConstructor());

    OF.emitTag("is_copy_ctor");
    OF.emitBoolean(D->isCopyConstructor());

    OF.emitTag("is_move_ctor");
    OF.emitBoolean(D->isMoveConstructor());

    OF.emitTag("is_converting_ctor");
    OF.emitBoolean(D->isConvertingConstructor(true));

    return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ClassTemplateDeclTupleSize() {
  return ASTExporter<ATDWriter>::RedeclarableTemplateDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitClassTemplateDecl(
    const ClassTemplateDecl *D) {
  ASTExporter<ATDWriter>::VisitRedeclarableTemplateDecl(D);

  VisitCXXRecordDecl(D->getTemplatedDecl());

  std::vector<const ClassTemplateSpecializationDecl *> DeclsToDump;
  llvm::SmallVector<ClassTemplatePartialSpecializationDecl*, 4> partials;
  ClassTemplateDecl *DNC = const_cast<ClassTemplateDecl*>(D);
  DNC->getPartialSpecializations(partials);
  TemplateParameterList *pList = D->getTemplateParameters();

  bool hasParams = !(pList->begin() == pList->end());
  bool hasPartials = !(partials.begin() == partials.end());

  if (D == D->getCanonicalDecl()) {
    // dump specializations once
    for (const auto *spec : D->specializations()) {
      switch (spec->getTemplateSpecializationKind()) {
      case TSK_Undeclared:
      case TSK_ImplicitInstantiation:
        DeclsToDump.push_back(spec);
        break;
      case TSK_ExplicitSpecialization:
      case TSK_ExplicitInstantiationDeclaration:
      case TSK_ExplicitInstantiationDefinition:
        // these specializations will be dumped elsewhere
        break;
      }
    }
  }

  bool ShouldDumpSpecializations = !DeclsToDump.empty();
  ObjectScope Scope(OF, 3);

  OF.emitTag("parameters");
  if (hasParams) {
      ArrayScope aScope(OF, pList->end() - pList->begin());
      for (auto &p : *pList) {
          TemplateTypeParmDecl *ttype = dyn_cast<TemplateTypeParmDecl>(p);
          NonTypeTemplateParmDecl *nt = dyn_cast<NonTypeTemplateParmDecl>(p);
          TemplateTemplateParmDecl *ttemp = dyn_cast<TemplateTemplateParmDecl>(p);
          
          if (ttype) {
              dumpTemplateTypeParmDecl(D, ttype);
          } else if (nt) {
              dumpNonTypeTemplateParmDecl(D, nt);
          } else if (ttemp) {
              dumpTemplateTemplateParmDecl(D, ttemp);
          }
      }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("specializations");
  if (ShouldDumpSpecializations) {
    ArrayScope aScope(OF, DeclsToDump.size());
    for (const auto *spec : DeclsToDump) {
      VisitClassTemplateSpecializationDecl(spec);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("partial_specializations");
  if (hasPartials) {
    ArrayScope aScope(OF, partials.end() - partials.begin());
    for (auto ptl : partials) {
      VisitClassTemplatePartialSpecializationDecl(ptl);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FunctionTemplateDeclTupleSize() {
  return ASTExporter<ATDWriter>::RedeclarableTemplateDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFunctionTemplateDecl(
    const FunctionTemplateDecl *D) {
  ASTExporter<ATDWriter>::VisitRedeclarableTemplateDecl(D);
  VisitFunctionDecl(D->getTemplatedDecl());
  std::vector<const FunctionDecl *> DeclsToDump;

  TemplateParameterList *pList = D->getTemplateParameters();
  bool hasParams = !(pList->begin() == pList->end());

  if (D == D->getCanonicalDecl()) {
    // dump specializations once
    for (const auto *spec : D->specializations()) {
      switch (spec->getTemplateSpecializationKind()) {
      case TSK_Undeclared:
      case TSK_ImplicitInstantiation:
      case TSK_ExplicitInstantiationDefinition:
      case TSK_ExplicitInstantiationDeclaration:
        DeclsToDump.push_back(spec);
        break;
      case TSK_ExplicitSpecialization:
        // these specializations will be dumped when they are defined
        break;
      }
    }
  }
  bool ShouldDumpSpecializations = !DeclsToDump.empty();
  ObjectScope Scope(OF, 2);

  OF.emitTag("parameters");
  if (hasParams) {
      ArrayScope aScope(OF, pList->end() - pList->begin());
      for (auto &p : *pList) {
          TemplateTypeParmDecl *ttype = dyn_cast<TemplateTypeParmDecl>(p);
          NonTypeTemplateParmDecl *nt = dyn_cast<NonTypeTemplateParmDecl>(p);
          TemplateTemplateParmDecl *ttemp = dyn_cast<TemplateTemplateParmDecl>(p);

          if (ttype) {
              dumpTemplateTypeParmDecl(D, ttype);
          } else if (nt) {
              dumpNonTypeTemplateParmDecl(D, nt);
          } else if (ttemp) {
              dumpTemplateTemplateParmDecl(D, ttemp);
          }
      }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("specializations");
  if (ShouldDumpSpecializations) {
    ArrayScope aScope(OF, DeclsToDump.size());
    for (const auto *spec : DeclsToDump) {
      dumpDecl(spec);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FriendDeclTupleSize() {
  return DeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFriendDecl(const FriendDecl *D) {
  VisitDecl(D);

  OF.emitTag("kind");
  if (TypeSourceInfo *T = D->getFriendType()) {
    OF.emitString("Type");

    OF.emitTag("type");
    dumpQualTypeNoQuals(T->getType());

    OF.emitTag("decl");
    {
      ObjectScope oScope(OF, 0);
    }

  } else {
    OF.emitString("Decl");

    OF.emitTag("type");
    {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("decl");
    dumpDecl(D->getFriendDecl());
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypeAliasDeclTupleSize() {
  return ASTExporter::TypedefNameDeclTupleSize() + 1;
}


template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTypeAliasDecl(const TypeAliasDecl *D) {
  ASTExporter<ATDWriter>::VisitTypedefNameDecl(D);

  TypeAliasTemplateDecl *dtemplate = D->getDescribedAliasTemplate();
  bool describes_template = dtemplate != nullptr;

  ObjectScope Scope(OF, 2);

  OF.emitTag("underlying_type");
  dumpQualType(D->getUnderlyingType());

  OF.emitTag("described_template");
  if (describes_template) {
    dumpPointer(dtemplate);
  } else  {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypeAliasTemplateDeclTupleSize() {
  return TypeAliasDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTypeAliasTemplateDecl(const
  TypeAliasTemplateDecl *D) {
  
  ASTExporter<ATDWriter>::VisitRedeclarableTemplateDecl(D);
  VisitTypeAliasDecl(D->getTemplatedDecl());

  TemplateParameterList *pList = D->getTemplateParameters();
  bool hasParams = !(pList->begin() == pList->end());

  TypeAliasTemplateDecl *member_template = D->getInstantiatedFromMemberTemplate();
  bool mTemp = member_template != nullptr;

  ObjectScope Scope(OF, 3);

  OF.emitTag("canonical_decl");
  dumpPointer(D->getCanonicalDecl());

  OF.emitTag("parameters");
  if (hasParams) {
      ArrayScope aScope(OF, pList->end() - pList->begin());
      for (auto &p : *pList) {
          TemplateTypeParmDecl *ttype = dyn_cast<TemplateTypeParmDecl>(p);
          NonTypeTemplateParmDecl *nt = dyn_cast<NonTypeTemplateParmDecl>(p);
          TemplateTemplateParmDecl *ttemp = dyn_cast<TemplateTemplateParmDecl>(p);

          if (ttype) {
              dumpTemplateTypeParmDecl(D, ttype);
          } else if (nt) {
              dumpNonTypeTemplateParmDecl(D, nt);
          } else if (ttemp) {
              dumpTemplateTemplateParmDecl(D, ttemp);
          }
      }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("member_template_decl");
  if (mTemp) {
      dumpPointer(member_template);
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitClassTemplatePartialSpecializationDecl(
  const ClassTemplatePartialSpecializationDecl *D) {
  VisitClassTemplateSpecializationDecl(D);
  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpTemplateTypeParmDecl(const
TemplateDecl *T, const TemplateTypeParmDecl *D) {

  bool const has_default = D->hasDefaultArgument();
  bool const used_typename = D->wasDeclaredWithTypename();
  bool const is_pack = D->isParameterPack();

  VisitTypeDecl(D);
  ObjectScope Scope(OF, 7);

  OF.emitTag("template_decl");
  dumpPointer(T);

  OF.emitTag("param_type");
  OF.emitString("TemplateTypeParam");

  OF.emitTag("with_typename");
  OF.emitBoolean(used_typename);

  unsigned int p_idx = D->getIndex();
  OF.emitTag("index");
  OF.emitInteger(p_idx);

  p_idx = D->getDepth();
  OF.emitTag("depth");
  OF.emitInteger(p_idx);

  OF.emitTag("is_parameter_pack");
  OF.emitBoolean(is_pack);

  OF.emitTag("default");
  if (has_default) {
    dumpQualType(D->getDefaultArgument());
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template<class ATDWriter>
void ASTExporter<ATDWriter>::dumpNonTypeTemplateParmDecl(const
TemplateDecl *T, const NonTypeTemplateParmDecl *D) {

    bool const has_default = D->hasDefaultArgument();
    bool const is_pack = D->isParameterPack();

    ASTExporter::VisitDeclaratorDecl(D);

    ObjectScope Scope(OF, 8);

    OF.emitTag("param_type");
    OF.emitString("TemplateNonTypeParam");

    OF.emitTag("template_decl");
    dumpPointer(T);

    unsigned int p_idx = D->getIndex();
    OF.emitTag("index");
    OF.emitInteger(p_idx);

    p_idx = D->getDepth();
    OF.emitTag("depth");
    OF.emitInteger(p_idx);

    OF.emitTag("has_default");
    OF.emitBoolean(has_default);
    OF.emitTag("is_parameter_pack");
    OF.emitBoolean(is_pack);
    OF.emitTag("type");
    dumpQualType(D->getType());

    OF.emitTag("default");
    if (has_default) {
      Expr const *def = D->getDefaultArgument();

      SourceLocation start_tok = def->getBeginLoc();
      SourceLocation end_tok = def->getEndLoc();
      unsigned int end_tok_len = Lexer::MeasureTokenLength(end_tok, 
              Context.getSourceManager(),
              Context.getLangOpts());
      SourceLocation end_tok_end = end_tok.getLocWithOffset(end_tok_len);

      SourceRange default_arg_range(start_tok, end_tok_end);
      OF.emitString(Lexer::getSourceText(
                  CharSourceRange::getCharRange(default_arg_range),
                  Context.getSourceManager(),
                  Context.getLangOpts())
              .str());
    } else {
      OF.emitString("None");
    }

    return;

}

template<class ATDWriter>
void ASTExporter<ATDWriter>::dumpTemplateTemplateParmDecl(const
TemplateDecl *T, const TemplateTemplateParmDecl *D) {

    bool const has_default = D->hasDefaultArgument();
    bool const is_pack = D->isParameterPack();
    unsigned int idx = D->getIndex();

    VisitNamedDecl(D);

    ObjectScope Scope(OF, 7);


    OF.emitTag("param_type");
    OF.emitString("TemplateTemplateParam");

    OF.emitTag("template_decl");
    dumpPointer(T);

    OF.emitTag("index");
    OF.emitInteger(idx);

    idx = D->getDepth();
    OF.emitTag("depth");
    OF.emitInteger(idx);

    OF.emitTag("has_default");
    OF.emitBoolean(has_default);

    OF.emitTag("is_parameter_pack");
    OF.emitBoolean(is_pack);

    OF.emitTag("default");
    if (has_default) {
        TemplateName def_temp = D->getDefaultArgument()
		.getArgument().getAsTemplateOrTemplatePattern();
        TemplateDecl *decl = def_temp.getAsTemplateDecl();
        dumpPointer(decl);
    }

    return;

}

//===----------------------------------------------------------------------===//
//  Stmt dumping methods.
//===----------------------------------------------------------------------===//

// Default aliases for generating variant components
// The main variant is defined at the end of section.
//#define STMT(CLASS, PARENT)
//#define ABSTRACT_STMT(STMT) STMT
//#include <clang/AST/StmtNodes.inc>
//
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpStmt(const Stmt *S) {

  ObjectScope oScope(OF, 2);
  if (!S) {
    // We use a fixed NullStmt node to represent null pointers
    S = NullPtrStmt;
  }

  OF.emitTag("kind");
  OF.emitString(std::string(S->getStmtClassName()));
  {
    ArrayScope aScope(OF, ASTExporter::tupleSizeOfStmtClass(S->getStmtClass()));
    ConstStmtVisitor<ASTExporter<ATDWriter>>::Visit(S);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::StmtTupleSize() {
  return 2;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitStmt(const Stmt *S) {
  {
    ObjectScope Scope(OF, 2);

    OF.emitTag("pointer");
    dumpPointer(S);
    OF.emitTag("source_range");
    dumpSourceRange(S->getSourceRange());
  }
  {
    ArrayScope Scope(OF, std::distance(S->child_begin(), S->child_end()));
    for (const Stmt *CI : S->children()) {
      dumpStmt(CI);
    }
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::DeclStmtTupleSize() {
  return StmtTupleSize() + 1;
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitDeclStmt(const DeclStmt *Node) {
  VisitStmt(Node);

  OF.emitTag("decls");
  ArrayScope Scope(OF, std::distance(Node->decl_begin(), Node->decl_end()));
  for (auto I : Node->decls()) {
    dumpDecl(I);
  }

  return;

}

////===----------------------------------------------------------------------===//
////  Expr dumping methods.
////===----------------------------------------------------------------------===//
//

template <class ATDWriter>
int ASTExporter<ATDWriter>::ExprTupleSize() {
  return StmtTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitExpr(const Expr *Node) {
  VisitStmt(Node);

  ExprValueKind VK = Node->getValueKind();
  bool HasNonDefaultValueKind = VK != VK_RValue;
  ExprObjectKind OK = Node->getObjectKind();
  bool HasNonDefaultObjectKind = OK != OK_Ordinary;
  ObjectScope Scope(OF, 3);

  OF.emitTag("qual_type");
  dumpQualType(Node->getType());

  OF.emitTag("value_kind");
  if (HasNonDefaultValueKind) {
    switch (VK) {
    case VK_LValue:
      OF.emitString("LValue");
      break;
    case VK_XValue:
      OF.emitString("XValue");
      break;
    case VK_RValue:
      llvm_unreachable("unreachable");
      break;
    }
  } else {
    OF.emitString("None");
  }


  OF.emitTag("object_kind");
  if (HasNonDefaultObjectKind) {
    switch (Node->getObjectKind()) {
    case OK_BitField:
      OF.emitString("BitField");
      break;
    case OK_ObjCProperty:
      OF.emitString("ObjCProperty");
      break;
    case OK_ObjCSubscript:
      OF.emitString("ObjCSubscript");
      break;
    case OK_VectorComponent:
      OF.emitString("VectorComponent");
      break;
    case OK_Ordinary:
      llvm_unreachable("unreachable");
      break;
    }
  } else {
    OF.emitString("None");
  }

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpCXXBaseSpecifier(
    const CXXBaseSpecifier &Base) {
  bool IsVirtual = Base.isVirtual();
  const CXXRecordDecl *RD =
      cast<CXXRecordDecl>(Base.getType()->getAs<RecordType>()->getDecl());
  ClassTemplateDecl *T = RD->getDescribedClassTemplate();
  bool describesTemplate = (T != nullptr);

  ObjectScope Scope(OF, 3);

  OF.emitTag("name");
  OF.emitString(RD->getName());

  OF.emitTag("template");
  if (describesTemplate) {
    dumpPointer(T);
  } else {
    ObjectScope oScope(OF, 0);
  }

  OF.emitTag("virtual");
  OF.emitBoolean(IsVirtual);
 
  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::DeclRefExprTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitDeclRefExpr(const DeclRefExpr *Node) {
  VisitExpr(Node);

  const ValueDecl *D = Node->getDecl();
  const NamedDecl *FD = Node->getFoundDecl();
  bool HasFoundDeclRef = FD && D != FD;
  ObjectScope Scope(OF, 2);

  OF.emitTag("decl_ref");
  if (D) {
    dumpDeclRef(*D);
  } else {
    ObjectScope oScope(OF, 0);
  }

  OF.emitTag("found_decl_ref");
  if (HasFoundDeclRef) {
    dumpDeclRef(*FD);
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::OverloadExprTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitOverloadExpr(const OverloadExpr *Node) {
  VisitExpr(Node);

  bool HasDecls = Node->getNumDecls() > 0;
  ObjectScope Scope(OF, 2); // not covered by tests

  OF.emitTag("decls");
  if (HasDecls) {
    ArrayScope Scope( // not covered by tests
        OF,
        std::distance(Node->decls_begin(), Node->decls_end()));
    for (auto I : Node->decls()) {
      dumpDeclRef(*I);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("name");
  dumpDeclarationName(Node->getName());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CharacterLiteralTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCharacterLiteral(
    const CharacterLiteral *Node) {
  VisitExpr(Node);

  OF.emitTag("value");
  OF.emitInteger(Node->getValue());

  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::emitAPInt(bool isSigned,
                                       const llvm::APInt &value) {
  ObjectScope Scope(OF, 3);

  OF.emitTag("is_signed");
  OF.emitBoolean(isSigned);

  OF.emitTag("bitwidth");
  OF.emitInteger(value.getBitWidth());

  OF.emitTag("value");
  OF.emitString(value.toString(10, isSigned));

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::IntegerLiteralTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitIntegerLiteral(const IntegerLiteral *Node) {
  VisitExpr(Node);

  const auto value = Node->getValue();

  OF.emitTag("value");
  this->emitAPInt(Node->getType()->isSignedIntegerType(), value);

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FixedPointLiteralTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFixedPointLiteral(
    const FixedPointLiteral *Node) {
  VisitExpr(Node);
  int radix = 10;

  OF.emitString("value");
  OF.emitString(Node->getValueAsString(radix));

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FloatingLiteralTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFloatingLiteral(const FloatingLiteral *Node) {
  VisitExpr(Node);
  llvm::SmallString<20> buf;
  Node->getValue().toString(buf);

  OF.emitTag("value");
  OF.emitString(buf.str());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::StringLiteralTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitStringLiteral(const StringLiteral *Str) {
  VisitExpr(Str);
  size_t n_chunks;
  if (Str->getByteLength() == 0) {
    n_chunks = 1;
  } else {
    n_chunks = 1 + ((Str->getByteLength() - 1) / Options.maxStringSize);
  }

  OF.emitTag("value");
  ArrayScope Scope(OF, n_chunks);
  for (size_t i = 0; i < n_chunks; ++i) {
    OF.emitString(Str->getBytes().substr(i * Options.maxStringSize,
                                         Options.maxStringSize));
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::MemberExprTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitMemberExpr(const MemberExpr *Node) {
  VisitExpr(Node);

  bool IsArrow = Node->isArrow();
  LangOptions LO;
  // ignore real lang options - it will get it wrong when compiling
  // with -fapple-kext flag
  bool PerformsVirtualDispatch = Node->performsVirtualDispatch(LO);
  ObjectScope Scope(OF, 4);

  OF.emitTag("is_arrow");
  OF.emitBoolean(IsArrow);

  OF.emitTag("performs_virtual_dispatch");
  OF.emitBoolean(PerformsVirtualDispatch);

  OF.emitTag("name");
  ValueDecl *memberDecl = Node->getMemberDecl();
  dumpName(*memberDecl);

  OF.emitTag("decl_ref");
  dumpDeclRef(*memberDecl);

  return;

}
////===----------------------------------------------------------------------===//
//// C++ Expressions
////===----------------------------------------------------------------------===//
template <class ATDWriter>
int ASTExporter<ATDWriter>::CXXDefaultArgExprTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCXXDefaultArgExpr(
    const CXXDefaultArgExpr *Node) {
  VisitExpr(Node);

  const Expr *InitExpr = Node->getExpr();
  ObjectScope Scope(OF, 1);

  OF.emitTag("init_expr");
  if (InitExpr) {
    dumpStmt(InitExpr);
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CXXDefaultInitExprTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCXXDefaultInitExpr(
    const CXXDefaultInitExpr *Node) {
  VisitExpr(Node);

  const Expr *InitExpr = Node->getExpr();
  ObjectScope Scope(OF, 1);

  OF.emitTag("init_expr");
  if (InitExpr) {
    dumpStmt(InitExpr);
  } else {
    ObjectScope(OF, 0);
  }

  return;

}

//===----------------------------------------------------------------------===//
// Comments
//===----------------------------------------------------------------------===//

template <class ATDWriter>
const char *ASTExporter<ATDWriter>::getCommandName(unsigned CommandID) {
  return Context.getCommentCommandTraits().getCommandInfo(CommandID)->Name;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpFullComment(const FullComment *C) {
  FC = C;
  comment_text = "";
  dumpComment(C);
  FC = 0;
}

//#define COMMENT(CLASS, PARENT) //@atd #define @CLASS@_tuple @PARENT@_tuple
//#define ABSTRACT_COMMENT(COMMENT) COMMENT
//#include <clang/AST/CommentNodes.inc>
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpComment(const Comment *C) {

  auto process_string = [](std::string to_process) -> std::string {
    int nons_begin = -1;
    int nons_end = -1;
    std::string out;

    char const *space = " ";

    for (unsigned int idx = 0; idx < to_process.size(); ++idx) {
      if (nons_begin < 0 && !strcmp(&to_process[idx], space)) {
        nons_begin = idx;
      }
      if (nons_begin >= 0 && !strcmp(&to_process[idx], space)) {
	nons_end = idx;
      }
    }

    for (int idx = nons_begin; idx < nons_end+1; ++idx) {
      out += to_process[idx];
    }

    return std::string(" ") + out;

  };

  OF.emitTag("kind");
  OF.emitString(std::string(C->getCommentKindName()));


  if (dyn_cast<TextComment>(C)) {
    comment_text += process_string(dyn_cast<TextComment>(C)->getText().str());
  } else if (dyn_cast<VerbatimBlockLineComment>(C)) {
    comment_text += process_string(dyn_cast<VerbatimBlockLineComment>(C)->getText().str());
  } else if (dyn_cast<VerbatimLineComment>(C)) {
    comment_text += process_string(dyn_cast<VerbatimLineComment>(C)->getText().str());
  } 

  if (!C) {
    // We use a fixed NoComment node to represent null pointers
    C = NullPtrComment;
  }

  OF.emitTag("parts");
  {
    ArrayScope Scope(OF,
                ASTExporter::tupleSizeOfCommentKind(C->getCommentKind()));
    ConstCommentVisitor<ASTExporter<ATDWriter>>::visit(C);
  }

  bool is_full_comment = (nullptr != dyn_cast<FullComment>(C));

  OF.emitTag("text");
  if (is_full_comment) {
    OF.emitString(comment_text);
  } else {
    OF.emitString("");
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CommentTupleSize() {
  return 3;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::visitComment(const Comment *C) {

    ObjectScope ObjComment(OF, 3); // not covered by tests
    OF.emitTag("parent_pointer");
    dumpPointer(C);
    OF.emitTag("source_range");
    dumpSourceRange(C->getSourceRange());
    OF.emitTag("comments");
  {
    Comment::child_iterator I = C->child_begin(), E = C->child_end();
    ArrayScope Scope(OF, std::distance(I, E));
    for (; I != E; ++I) {
      dumpComment(*I);
    }
  }
  
  return;

}

// PICKUP HERE
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpType(const Type *T) {

  ObjectScope oScope(OF);

  std::string typeClassName = T ? T->getTypeClassName() : "None";

  OF.emitTag("kind");
  OF.emitString(typeClassName + "Type");

  {
    if (T) {
      // TypeVisitor assumes T is non-null
      ArrayScope Scope(OF,
                       ASTExporter::tupleSizeOfTypeClass(T->getTypeClass()));
      TypeVisitor<ASTExporter<ATDWriter>>::Visit(T);
    } else {
      ArrayScope Scope(OF, 1);
      VisitType(nullptr);
    }
  }
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpPointerToType(const Type *T) {
  dumpPointer(T);
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpQualTypeNoQuals(const QualType &qt) {
  const Type *T = qt.getTypePtrOrNull();
  dumpPointerToType(T);
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypeTupleSize() {
  return 1;
}
template <class ATDWriter>
int ASTExporter<ATDWriter>::TypeWithChildInfoTupleSize() {
  return 2;
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitType(const Type *T) {
  // NOTE: T can (and will) be null here!!

  bool HasDesugaredType = T && T->getUnqualifiedDesugaredType() != T;
  ObjectScope Scope(OF, 2);

  OF.emitTag("pointer");
  dumpPointer(T);

  OF.emitTag("desugared_type");
  if (HasDesugaredType) {
    dumpPointerToType(T->getUnqualifiedDesugaredType());
  } else {
    ObjectScope oScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::AdjustedTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitAdjustedType(const AdjustedType *T) {
  VisitType(T);

  OF.emitTag("qual_type");
  dumpQualType(T->getAdjustedType());
  
  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ArrayTypeTupleSize() {
  return TypeTupleSize() + 1;
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitArrayType(const ArrayType *T) {
  VisitType(T);
  QualType EltT = T->getElementType();
  bool HasStride = hasMeaningfulTypeInfo(EltT.getTypePtr());
  ObjectScope Scope(OF, 2);
  OF.emitTag("element_type");
  dumpQualType(EltT);

  OF.emitTag("stride");
  if (HasStride) {
    OF.emitInteger(Context.getTypeInfo(EltT).Width / 8);
  } else {
    OF.emitString("None");
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ConstantArrayTypeTupleSize() {
  return ArrayTypeTupleSize() + 1;
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitConstantArrayType(
    const ConstantArrayType *T) {
  VisitArrayType(T);

  OF.emitTag("size");
  OF.emitInteger(T->getSize().getLimitedValue());
  
  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::VariableArrayTypeTupleSize() {
  return ArrayTypeTupleSize() + 1;
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitVariableArrayType(
    const VariableArrayType *T) {
  VisitArrayType(T);

  OF.emitTag("pointer");
  dumpPointer(T->getSizeExpr());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::AtomicTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitAtomicType(const AtomicType *T) {
  VisitType(T);

  OF.emitTag("qual_type");
  dumpQualType(T->getValueType());

  return;

}
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpAttrKind(attr::Kind Kind) {
  
  switch (Kind) {
#define ATTR(NAME)                          \
  case AttributedType::Kind::NAME:          \
    OF.emitSimpleVariant(#NAME "AttrKind"); \
    return;
#include <clang/Basic/AttrList.inc>
  }
  llvm_unreachable("Attribute kind that is not part of AttrList.inc!");

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::AttributedTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitAttributedType(const AttributedType *T) {
  VisitType(T);

  ObjectScope Scope(OF, 1);

  OF.emitTag("attr_kind");
  dumpAttrKind(T->getAttrKind());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::BlockPointerTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitBlockPointerType(const BlockPointerType *T) {
  VisitType(T);

  OF.emitTag("qual_type");
  dumpQualType(T->getPointeeType());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::BuiltinTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitBuiltinType(const BuiltinType *T) {
  VisitType(T);
  std::string type_name;
  switch (T->getKind()) {
#define BUILTIN_TYPE(TYPE, ID) \
  case BuiltinType::TYPE: {    \
    type_name = #TYPE;         \
    break;                     \
  }
#include <clang/AST/BuiltinTypes.def>
#define IMAGE_TYPE(ImgType, ID, SingletonId, Access, Suffix) \
  case BuiltinType::ID:
#include <clang/Basic/OpenCLImageTypes.def>
#define EXT_OPAQUE_TYPE(Name, Id, Ext) case BuiltinType::Id:
#include <clang/Basic/OpenCLExtensionTypes.def>
    llvm_unreachable("OCL builtin types are unsupported");
    break;
  }

  OF.emitTag("type_name");
  OF.emitString(type_name);
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::DecltypeTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitDecltypeType(const DecltypeType *T) {
  VisitType(T);

  OF.emitTag("qual_type");
  dumpQualType(T->getUnderlyingType());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FunctionTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFunctionType(const FunctionType *T) {
  VisitType(T);

  OF.emitTag("return_type");
  dumpQualType(T->getReturnType());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FunctionProtoTypeTupleSize() {
  return FunctionTypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFunctionProtoType(
    const FunctionProtoType *T) {
  VisitFunctionType(T);

  bool HasParamsType = T->getNumParams() > 0;
  ObjectScope Scope(OF, 1);

  OF.emitTag("params_type");
  if (HasParamsType) {
    ArrayScope aScope(OF, T->getParamTypes().size());
    for (const auto &paramType : T->getParamTypes()) {
      dumpQualType(paramType);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::MemberPointerTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitMemberPointerType(
    const MemberPointerType *T) {
  VisitType(T);

  OF.emitTag("qual_type");
  dumpQualType(T->getPointeeType());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ParenTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitParenType(const ParenType *T) {
  // this is just syntactic sugar
  VisitType(T);
  
  OF.emitTag("qual_type");
  dumpQualType(T->getInnerType());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::PointerTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitPointerType(const PointerType *T) {
  VisitType(T);

  OF.emitTag("qual_type");
  dumpQualType(T->getPointeeType());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ReferenceTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitReferenceType(const ReferenceType *T) {
  VisitType(T);

  OF.emitTag("qual_type");
  dumpQualType(T->getPointeeType());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TagTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTagType(const TagType *T) {
  VisitType(T);

  OF.emitTag("pointer");
  dumpPointer(T->getDecl());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypedefTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTypedefType(const TypedefType *T) {
  VisitType(T);
  OF.emitTag("child_type");
  dumpQualType(T->desugar());
  OF.emitTag("decl_ptr");
  dumpPointer(T->getDecl());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TemplateTypeParmTypeTupleSize() {
    return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTemplateTypeParmType(const TemplateTypeParmType *T) {
    VisitType(T);
    
    bool isSugared = T->isSugared();
    ObjectScope Scope(OF, 6);

    OF.emitTag("id");

    IdentifierInfo *id = T->getIdentifier();
    if (id) {
        OF.emitString(id->getName().str());
    } else {
        OF.emitString("");
    }

    OF.emitTag("depth");
    OF.emitInteger(T->getDepth());

    OF.emitTag("index");
    OF.emitInteger(T->getIndex());

    OF.emitTag("is_pack");
    OF.emitBoolean(T->isParameterPack());

    OF.emitTag("parm");
    dumpPointer(T->getDecl());

    OF.emitTag("desugared_type");
    if (isSugared) {
        dumpQualType(T->desugar());
    } else {
      ObjectScope oScope(OF, 0);
    }

    return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::SubstTemplateTypeParmTypeTupleSize() {
    return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitSubstTemplateTypeParmType(const SubstTemplateTypeParmType *T) {
    VisitType(T);

    bool isSugared = T->isSugared();
    ObjectScope Scope(OF, 3);

    OF.emitTag("replaced");
    dumpPointerToType(T->getReplacedParameter());

    OF.emitTag("replacement_type");
    dumpQualType(T->getReplacementType());

    OF.emitTag("desugared_type");
    if (isSugared) {
        dumpQualType(T->desugar());
    } else {
      ObjectScope oScope(OF, 0);
    }

    return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TemplateSpecializationTypeTupleSize() {
    return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTemplateSpecializationType(const TemplateSpecializationType *T) {
    VisitType(T);

    bool isSugared = T->isSugared();
    bool isAlias = T->isTypeAlias();
    unsigned int nArgs = T->getNumArgs();
    bool hasArgs = nArgs > 0;

    ObjectScope Scope(OF, 5);

    OF.emitTag("type_alias");
    OF.emitBoolean(isAlias);

    OF.emitTag("template_decl");
    dumpPointer(T->getTemplateName().getAsTemplateDecl());

    OF.emitTag("aliased_type");
    if (isAlias) {
        dumpQualType(T->getAliasedType());
    } else {
      ObjectScope oScope(OF, 0);
    }

    OF.emitTag("desugared_type");
    if (isSugared) {
        dumpQualType(T->desugar());
    } else {
      ObjectScope oScope(OF, 0);
    }

    TemplateArgument const *args = T->getArgs();

    OF.emitTag("specialization_args");
    if (args && hasArgs) {
        ArrayScope aScope(OF, nArgs);
        for (unsigned int arg_idx = 0; arg_idx < nArgs; ++arg_idx) {
            dumpTemplateArgument(args[arg_idx]);
        }
    } else {
      ArrayScope aScope(OF, 0);
    }

    return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::InjectedClassNameTypeTupleSize() {
    return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitInjectedClassNameType(const InjectedClassNameType *T) {
    VisitType(T);

    bool isSugared = T->isSugared();
    ObjectScope Scope(OF, 2);

    OF.emitTag("injected_specialization_type");
    dumpQualType(T->getInjectedSpecializationType());

    OF.emitTag("desugared_type");
    if (isSugared) {
        dumpQualType(T->desugar());
    } else {
      ObjectScope oScope(OF, 0);
    }

    return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::DependentNameTypeTupleSize() {
    return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitDependentNameType(const DependentNameType *T) {
    VisitType(T);

    bool isSugared = T->isSugared();
    ObjectScope Scope(OF, 2);

    OF.emitTag("identifier");
    OF.emitString(T->getIdentifier()->getName().str());

    OF.emitTag("desugared_type");
    if (isSugared) {
        dumpQualType(T->desugar());
    } else {
      ObjectScope oScope(OF, 0);
    }

    return;

}

//===----------------------------------------------------------------------===//
//  Attr dumping methods.
//===----------------------------------------------------------------------===//

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpAttr(const Attr *A) {
  std::string tag;
  switch (A->getKind()) {
#define ATTR(NAME)       \
  case attr::Kind::NAME: \
    tag = #NAME "Attr";  \
    break;
#include <clang/Basic/AttrList.inc>
  }

  OF.emitTag("kind");
  OF.emitString(tag);
  {
    ArrayScope Scope(OF, ASTExporter::tupleSizeOfAttrKind(A->getKind()));
    ConstAttrVisitor<ASTExporter<ATDWriter>>::Visit(A);
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::AttrTupleSize() {
  return 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitAttr(const Attr *A) {
  ObjectScope Scope(OF, 3);
  OF.emitTag("pointer");
  dumpPointer(A);
  OF.emitTag("source_range");
  dumpSourceRange(A->getRange());
  OF.emitTag("attr");
  OF.emitString(std::string(A->getSpelling()));

  return;

}
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpVersionTuple(const VersionTuple &VT) {
  Optional<unsigned> minor = VT.getMinor();
  Optional<unsigned> subminor = VT.getSubminor();
  Optional<unsigned> build = VT.getBuild();
  ObjectScope Scope(
      OF, 4);

  OF.emitTag("major");
  OF.emitInteger(VT.getMajor());

  OF.emitTag("minor");
  if (minor.hasValue()) {
    OF.emitInteger(minor.getValue());
  } else {
    OF.emitString("None");
  }

  OF.emitTag("subminor");
  if (subminor.hasValue()) {
    OF.emitInteger(subminor.getValue());
  } else {
    OF.emitString("None");
  }

  OF.emitTag("build");
  if (build.hasValue()) {
    OF.emitInteger(build.getValue());
  } else {
    OF.emitString("None");
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::AnnotateAttrTupleSize() {
  return AttrTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitAnnotateAttr(const AnnotateAttr *A) {
  VisitAttr(A);

  OF.emitTag("annotation");
  OF.emitString(A->getAnnotation().str());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::AvailabilityAttrTupleSize() {
  return AttrTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitAvailabilityAttr(const AvailabilityAttr *A) {
  VisitAttr(A);
  {
    IdentifierInfo *platform = A->getPlatform();
    ObjectScope Scope(OF, 3);

    OF.emitTag("platform");
    if (platform != nullptr) {
      OF.emitString(platform->getNameStart());
    } else {
      OF.emitString("None");
    }

    OF.emitTag("introduced");
    dumpVersionTuple(A->getIntroduced());
  }

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::SentinelAttrTupleSize() {
  return AttrTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitSentinelAttr(const SentinelAttr *A) {
  VisitAttr(A);
  ObjectScope Scope(OF, 2);

  OF.emitTag("sentinel");
  OF.emitInteger(A->getSentinel());
  OF.emitTag("null_pos");
  OF.emitInteger(A->getNullPos());

  return;

}

template <class ATDWriter>
int ASTExporter<ATDWriter>::VisibilityAttrTupleSize() {
  return AttrTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitVisibilityAttr(const VisibilityAttr *A) {
  VisitAttr(A);

  OF.emitTag("kind");
  switch (A->getVisibility()) {
  case VisibilityAttr::Default:
    OF.emitString("DefaultVisibility");
    break;
  case VisibilityAttr::Hidden:
    OF.emitString("HiddenVisibility");
    break;
  case VisibilityAttr::Protected:
    OF.emitString("ProtectedVisibility");
    break;
  }

  return;

}

template <class ATDWriter = JsonWriter>
class ExporterASTConsumer : public ASTConsumer {
 private:
  std::shared_ptr<ASTExporterOptions> options;
  std::unique_ptr<raw_ostream> OS;

 public:
  using ASTConsumerOptions = ASTLib::ASTExporterOptions;
  using PreprocessorHandler = ASTPluginLib::EmptyPreprocessorHandler;
  using PreprocessorHandlerData = ASTPluginLib::EmptyPreprocessorHandlerData;

  ExporterASTConsumer(const CompilerInstance &CI,
                      std::shared_ptr<ASTConsumerOptions> options,
                      std::shared_ptr<PreprocessorHandlerData> sharedData,
                      std::unique_ptr<raw_ostream> &&OS)
      : options(options), OS(std::move(OS)) {
  }

  virtual void HandleTranslationUnit(ASTContext &Context) {
    TranslationUnitDecl *D = Context.getTranslationUnitDecl();
    ASTExporter<ATDWriter> P(*OS, Context, *options);
    P.dumpDecl(D);
  }
};

typedef ASTPluginLib::SimplePluginASTAction<
    ASTLib::ExporterASTConsumer<ASTLib::JsonWriter>>
    JsonExporterASTAction;

} // end of namespace ASTLib


// Unused



#endif
