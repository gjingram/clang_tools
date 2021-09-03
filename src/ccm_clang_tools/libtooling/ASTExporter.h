/**
 * Copyright (c) 2014, Facebook, Inc.  * Copyright (c) 2003-2014 University of
 * Illinois at Urbana-Champaign. All rights reserved.
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

#include <cstring>
#include <memory>
#include <set>
#include <list>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Attr.h>
#include <clang/AST/AttrVisitor.h>
#include <clang/AST/CommentVisitor.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclLookups.h>
#include <clang/AST/DeclObjC.h>
#include <clang/AST/DeclTemplate.h>
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
#include <string>

//===----------------------------------------------------------------------===//
// ASTExporter Visitor
//===----------------------------------------------------------------------===//

namespace ASTLib {

struct ASTExporterOptions : ASTPluginLib::PluginASTOptionsBase {
  bool withPointers = true;
  bool dumpComments = true;
  bool useMacroExpansionLocation = true;
  JSONWriter::JSONWriterOptions jsonWriterOptions = {.prettifyJson = true};

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
  const ASTPluginLib::IncludesPreprocessorHandlerData &PreProcessor;

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
  bool building_comment;

  NamePrinter<ATDWriter> NamePrint;

  std::list<const Decl*>        decls_referenced;
  std::list<const Type*>        types_referenced;
  std::vector<const Decl*>      new_decls_referenced;
  std::vector<const Type*>      new_types_referenced;
  std::list<const Decl*>        decls_written;
  std::list<const Type*>        types_written;
  std::vector<const Decl*>      decls_inherited;
  bool parsing_refs = false;

 public:
  ASTExporter(raw_ostream &OS,
              ASTContext &Context,
              const ASTExporterOptions &Opts,
              const ASTPluginLib::IncludesPreprocessorHandlerData &PreProc)
      : OF(OS, Opts.jsonWriterOptions),
        Context(Context),
        Options(Opts),
        PreProcessor(PreProc),
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
  void dumpDecl(const Decl *D, bool force = false);
  void dumpStmt(const Stmt *S);
  void dumpFullComment(const FullComment *C);
  void dumpType(const Type *T);
  void dumpPointerToType(const Type *T);
  void dumpQualTypeNoQuals(const QualType &qt);
  void dumpClassLambdaCapture(const LambdaCapture *C);
  void dumpVersionTuple(const VersionTuple &VT);

  // Utilities
  void dumpPointer(const void *Ptr);
  void dumpDeclPointer(const Decl *Ptr);
  bool declIsHidden(const Decl *Ptr);
  bool typeIsHidden(const QualType &qt);
  void dumpSourceRange(SourceRange R);
  void dumpSourceLocation(SourceLocation Loc);
  void dumpQualType(const QualType &qt);
  void dumpTypeOld(const Type *T);
  void dumpDeclRef(const Decl &Node, bool reference = true);
  bool hasNodes(const DeclContext *DC);
  void dumpLookups(const DeclContext &DC);
  void dumpSelector(const Selector sel);
  void dumpName(const NamedDecl &decl);
  void dumpInputKind(const InputKind kind);
  void dumpIntegerTypeWidths(const TargetInfo &info);
  void dumpDefaultArgStr(const Expr*);

  bool alwaysEmitParent(const Decl *D);
  bool inMainFile(const Decl *D);
  bool declWritten(const Decl *D);
  bool typeWritten(const Type *T);
  void referenceDecl(const Decl *D);
  bool declIsReferenced(const Decl *D);
  bool declIsInherited(const Decl *D);
  int contextCanBlock(const DeclContext *DC);
  int declCanWrite(const Decl *D);

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
  void dumpCXXBaseSpecifier(const CXXBaseSpecifier &Base);
  void dumpTemplateParameters(const TemplateDecl *D,
                              const TemplateParameterList *TPL);

#define DECLARE_VISITOR(NAME) \
  int NAME##TupleSize();      \
  void Visit##NAME(const NAME *D);

#define DECLARE_LOWERCASE_VISITOR(NAME) \
  int NAME##TupleSize();                \
  void visit##NAME(const NAME *D);

#define NO_DECL_IMPL(NAME)                 \
  int NAME##TupleSize() { return 1; } \
  void Visit##NAME(const NAME *D) { \
      OF.emitTag("skipped"); \
      OF.emitBoolean(true); \
      OF.emitTag("reason"); \
      OF.emitString("Visitor for " #NAME " not implemented"); \
      if (dyn_cast<NamedDecl>(D)) { \
          OF.emitTag("id"); \
          ObjectScope oScope(OF, 1); \
          dumpName(*cast<NamedDecl>(D)); \
      } \
      OF.emitTag("pointer"); \
      dumpPointer(D); \
      return; \
  }

#define NO_IMPL(NAME)                 \
  int NAME##TupleSize() { return 1; } \
  void Visit##NAME(const NAME *D) { \
      OF.emitTag("skipped"); \
      OF.emitBoolean(true); \
      OF.emitTag("reason"); \
      OF.emitString("Visitor for " #NAME " not implemented"); \
      OF.emitTag("pointer"); \
      dumpPointer(D); \
      return; \
  }

#define NO_LOWERCASE_IMPL(NAME)        \
  int NAME##TupleSize() { return -1; } \
  void visit##NAME(const NAME *D) { return; }

  // Decls
  DECLARE_VISITOR(Decl)
  DECLARE_VISITOR(DeclContext)
  NO_DECL_IMPL(AccessSpecDecl)
  NO_DECL_IMPL(BlockDecl)
  DECLARE_VISITOR(CapturedDecl)
  NO_DECL_IMPL(ClassScopeFunctionSpecializationDecl)
  NO_DECL_IMPL(EmptyDecl)
  NO_DECL_IMPL(ExportDecl)
  NO_DECL_IMPL(ExternCContextDecl)
  NO_DECL_IMPL(FileScopeAsmDecl)
  DECLARE_VISITOR(FriendDecl)
  NO_DECL_IMPL(FriendTemplateDecl)
  DECLARE_VISITOR(ImportDecl)
  NO_DECL_IMPL(LifetimeExtendedTemporaryDecl)
  DECLARE_VISITOR(LinkageSpecDecl)
  DECLARE_VISITOR(NamedDecl)
  NO_DECL_IMPL(LabelDecl)
  DECLARE_VISITOR(NamespaceDecl)
  DECLARE_VISITOR(NamespaceAliasDecl)
  NO_DECL_IMPL(ObjCCompatibleAliasDecl)
  NO_DECL_IMPL(ObjCContainerDecl)
  NO_DECL_IMPL(ObjCCategoryDecl)
  NO_DECL_IMPL(ObjCImplDecl)
  NO_DECL_IMPL(ObjCCategoryImplDecl)
  NO_DECL_IMPL(ObjCImplementationDecl)
  NO_DECL_IMPL(ObjCInterfaceDecl)
  NO_DECL_IMPL(ObjCProtocolDecl)
  NO_DECL_IMPL(ObjCMethodDecl)
  NO_DECL_IMPL(ObjCPropertyDecl)
  NO_DECL_IMPL(TemplateDecl)
  NO_DECL_IMPL(BuiltinTemplateDecl)
  NO_DECL_IMPL(ConceptDecl)
  NO_DECL_IMPL(RedeclarableTemplateDecl)
  DECLARE_VISITOR(ClassTemplateDecl)
  DECLARE_VISITOR(FunctionTemplateDecl)
  DECLARE_VISITOR(TypeAliasTemplateDecl)
  NO_DECL_IMPL(VarTemplateDecl)
  NO_DECL_IMPL(TemplateTemplateParmDecl)
  DECLARE_VISITOR(TypeDecl)
  DECLARE_VISITOR(TagDecl)
  DECLARE_VISITOR(RecordDecl)
  DECLARE_VISITOR(EnumDecl)
  DECLARE_VISITOR(CXXRecordDecl)
  DECLARE_VISITOR(ClassTemplateSpecializationDecl)
  // Custom
  // NO_DECL_IMPL(ClassTemplatePartialSpecialization)
  NO_DECL_IMPL(TemplateTypeParmDecl)
  NO_DECL_IMPL(TypedefNameDecl)
  NO_DECL_IMPL(ObjCTypeParamDecl)
  DECLARE_VISITOR(TypeAliasDecl)
  DECLARE_VISITOR(TypedefDecl)
  NO_DECL_IMPL(UnresolvedUsingTypenameDecl)
  NO_DECL_IMPL(UsingDecl)
  DECLARE_VISITOR(UsingDirectiveDecl)
  NO_DECL_IMPL(UsingPackDecl)
  NO_DECL_IMPL(UsingShadowDecl)
  NO_DECL_IMPL(ConstructorUsingShadowDecl)
  DECLARE_VISITOR(ValueDecl)
  NO_DECL_IMPL(BindingDecl)
  NO_DECL_IMPL(DeclaratorDecl)
  DECLARE_VISITOR(FieldDecl)
  NO_DECL_IMPL(ObjCAtDefsFieldDecl)
  NO_DECL_IMPL(ObjCIvarDecl)
  DECLARE_VISITOR(FunctionDecl)
  NO_DECL_IMPL(CXXDeductionGuideDecl)
  DECLARE_VISITOR(CXXMethodDecl)
  DECLARE_VISITOR(CXXConstructorDecl)
  NO_DECL_IMPL(CXXConversionDecl)
  NO_DECL_IMPL(CXXDestructorDecl)
  NO_DECL_IMPL(MSPropertyDecl)
  NO_DECL_IMPL(NonTypeTemplateParmDecl)
  DECLARE_VISITOR(VarDecl)
  NO_DECL_IMPL(DecompositionDecl)
  NO_DECL_IMPL(ImplicitParamDecl)
  NO_DECL_IMPL(OMPCapturedExprDecl)
  DECLARE_VISITOR(ParmVarDecl)
  NO_DECL_IMPL(VarTemplateSpecializationDecl)
  NO_DECL_IMPL(VarTemplatePartialSpecializationDecl)
  DECLARE_VISITOR(EnumConstantDecl)
  DECLARE_VISITOR(IndirectFieldDecl)
  NO_DECL_IMPL(OMPDeclareMapperDecl)
  NO_DECL_IMPL(OMPDeclareReductionDecl)
  NO_DECL_IMPL(UnresolvedUsingValueDecl)
  NO_DECL_IMPL(OMPAllocateDecl)
  NO_DECL_IMPL(OMPRequiresDecl)
  NO_DECL_IMPL(OMPThreadPrivateDecl)
  NO_DECL_IMPL(ObjCPropertyImplDecl)
  NO_DECL_IMPL(PragmaCommentDecl)
  NO_DECL_IMPL(PragmaDetectMismatchDecl)
  NO_DECL_IMPL(RequiresExprBodyDecl)
  NO_DECL_IMPL(StaticAssertDecl)
  DECLARE_VISITOR(TranslationUnitDecl)

  void VisitClassTemplatePartialSpecializationDecl(
      const ClassTemplatePartialSpecializationDecl *D);

  // Stmts.
  DECLARE_VISITOR(Stmt)
  NO_IMPL(AsmStmt)
  NO_IMPL(GCCAsmStmt)
  NO_IMPL(MSAsmStmt)
  NO_IMPL(BreakStmt)
  NO_IMPL(CXXCatchStmt)
  NO_IMPL(CXXForRangeStmt)
  NO_IMPL(CXXTryStmt)
  NO_IMPL(CapturedStmt)
  NO_IMPL(CompoundStmt)
  NO_IMPL(ContinueStmt)
  NO_IMPL(CoreturnStmt)
  NO_IMPL(CoroutineBodyStmt)
  DECLARE_VISITOR(DeclStmt)
  NO_IMPL(DoStmt)
  NO_IMPL(ForStmt)
  NO_IMPL(GotoStmt)
  NO_IMPL(IfStmt)
  NO_IMPL(IndirectGotoStmt)
  NO_IMPL(MSDependentExistsStmt)
  NO_IMPL(NullStmt)
  NO_IMPL(OMPExecutableDirective)
  NO_IMPL(OMPAtomicDirective)
  NO_IMPL(OMPBarrierDirective)
  NO_IMPL(OMPCancelDirective)
  NO_IMPL(OMPCancellationPointDirective)
  NO_IMPL(OMPCriticalDirective)
  NO_IMPL(OMPFlushDirective)
  NO_IMPL(OMPLoopDirective)
  NO_IMPL(OMPDistributeDirective)
  NO_IMPL(OMPDistributeParallelForDirective)
  NO_IMPL(OMPDistributeParallelForSimdDirective)
  NO_IMPL(OMPDistributeSimdDirective)
  NO_IMPL(OMPForDirective)
  NO_IMPL(OMPForSimdDirective)
  NO_IMPL(OMPMasterTaskLoopDirective)
  NO_IMPL(OMPMasterTaskLoopSimdDirective)
  NO_IMPL(OMPParallelForDirective)
  NO_IMPL(OMPParallelForSimdDirective)
  NO_IMPL(OMPParallelMasterTaskLoopDirective)
  NO_IMPL(OMPParallelMasterTaskLoopSimdDirective)
  NO_IMPL(OMPSimdDirective)
  NO_IMPL(OMPTargetParallelForSimdDirective)
  NO_IMPL(OMPTargetSimdDirective)
  NO_IMPL(OMPTargetTeamsDistributeDirective)
  NO_IMPL(OMPTargetTeamsDistributeParallelForDirective)
  NO_IMPL(OMPTargetTeamsDistributeParallelForSimdDirective)
  NO_IMPL(OMPTargetTeamsDistributeSimdDirective)
  NO_IMPL(OMPTaskLoopDirective)
  NO_IMPL(OMPTaskLoopSimdDirective)
  NO_IMPL(OMPTeamsDistributeDirective)
  NO_IMPL(OMPTeamsDistributeParallelForDirective)
  NO_IMPL(OMPTeamsDistributeParallelForSimdDirective)
  NO_IMPL(OMPTeamsDistributeSimdDirective)
  NO_IMPL(OMPMasterDirective)
  NO_IMPL(OMPOrderedDirective)
  NO_IMPL(OMPParallelDirective)
  NO_IMPL(OMPParallelMasterDirective)
  NO_IMPL(OMPParallelSectionsDirective)
  NO_IMPL(OMPSectionDirective)
  NO_IMPL(OMPSectionsDirective)
  NO_IMPL(OMPSingleDirective)
  NO_IMPL(OMPTargetDataDirective)
  NO_IMPL(OMPTargetDirective)
  NO_IMPL(OMPTargetEnterDataDirective)
  NO_IMPL(OMPTargetExitDataDirective)
  NO_IMPL(OMPTargetParallelDirective)
  NO_IMPL(OMPTargetParallelForDirective)
  NO_IMPL(OMPTargetTeamsDirective)
  NO_IMPL(OMPTargetUpdateDirective)
  NO_IMPL(OMPTaskDirective)
  NO_IMPL(OMPTaskgroupDirective)
  NO_IMPL(OMPTaskwaitDirective)
  NO_IMPL(OMPTaskyieldDirective)
  NO_IMPL(OMPTeamsDirective)
  NO_IMPL(ObjCAtCatchStmt)
  NO_IMPL(ObjCAtFinallyStmt)
  NO_IMPL(ObjCAtSynchronizedStmt)
  NO_IMPL(ObjCAtThrowStmt)
  NO_IMPL(ObjCAtTryStmt)
  NO_IMPL(ObjCAutoreleasePoolStmt)
  NO_IMPL(ObjCForCollectionStmt)
  NO_IMPL(ReturnStmt)
  NO_IMPL(SEHExceptStmt)
  NO_IMPL(SEHFinallyStmt)
  NO_IMPL(SEHLeaveStmt)
  NO_IMPL(SEHTryStmt)
  NO_IMPL(SwitchCase)
  NO_IMPL(CaseStmt)
  NO_IMPL(DefaultStmt)
  NO_IMPL(SwitchStmt)
  NO_IMPL(ValueStmt)

  // Exprs
  DECLARE_VISITOR(Expr)
  NO_IMPL(AbstractConditionalOperator)
  NO_IMPL(BinaryConditionalOperator)
  NO_IMPL(ConditionalOperator)
  NO_IMPL(AddrLabelExpr)
  NO_IMPL(ArrayInitIndexExpr)
  NO_IMPL(ArrayInitLoopExpr)
  NO_IMPL(ArraySubscriptExpr)
  NO_IMPL(ArrayTypeTraitExpr)
  NO_IMPL(AsTypeExpr)
  NO_IMPL(AtomicExpr)
  NO_IMPL(BinaryOperator)
  NO_IMPL(CompoundAssignOperator)
  NO_IMPL(BlockExpr)
  NO_IMPL(CXXBindTemporaryExpr)
  NO_IMPL(CXXBoolLiteralExpr)
  NO_IMPL(CXXConstructExpr)
  NO_IMPL(CXXTemporaryObjectExpr)
  DECLARE_VISITOR(CXXDefaultArgExpr)
  DECLARE_VISITOR(CXXDefaultInitExpr)
  NO_IMPL(CXXDeleteExpr)
  NO_IMPL(CXXDependentScopeMemberExpr)
  NO_IMPL(CXXFoldExpr)
  NO_IMPL(CXXInheritedCtorInitExpr)
  NO_IMPL(CXXNewExpr)
  NO_IMPL(CXXNoexceptExpr)
  NO_IMPL(CXXNullPtrLiteralExpr)
  NO_IMPL(CXXPseudoDestructorExpr)
  NO_IMPL(CXXRewrittenBinaryOperator)
  NO_IMPL(CXXScalarValueInitExpr)
  NO_IMPL(CXXStdInitializerListExpr)
  NO_IMPL(CXXThisExpr)
  NO_IMPL(CXXThrowExpr)
  NO_IMPL(CXXTypeidExpr)
  NO_IMPL(CXXUnresolvedConstructExpr)
  NO_IMPL(CXXUuidofExpr)
  NO_IMPL(CallExpr)
  NO_IMPL(CUDAKernelCallExpr)
  NO_IMPL(CXXMemberCallExpr)
  NO_IMPL(CXXOperatorCallExpr)
  NO_IMPL(UserDefinedLiteral)
  NO_IMPL(CastExpr)
  NO_IMPL(ExplicitCastExpr)
  NO_IMPL(BuiltinBitCastExpr)
  NO_IMPL(CStyleCastExpr)
  NO_IMPL(CXXFunctionalCastExpr)
  NO_IMPL(CXXNamedCastExpr)
  NO_IMPL(CXXConstCastExpr)
  NO_IMPL(CXXDynamicCastExpr)
  NO_IMPL(CXXReinterpretCastExpr)
  NO_IMPL(CXXStaticCastExpr)
  NO_IMPL(ObjCBridgedCastExpr)
  NO_IMPL(ImplicitCastExpr)
  DECLARE_VISITOR(CharacterLiteral)
  NO_IMPL(ChooseExpr)
  NO_IMPL(CompoundLiteralExpr)
  NO_IMPL(ConceptSpecializationExpr)
  NO_IMPL(ConvertVectorExpr)
  NO_IMPL(CoroutineSuspendExpr)
  NO_IMPL(CoawaitExpr)
  NO_IMPL(CoyieldExpr)
  DECLARE_VISITOR(DeclRefExpr)
  NO_IMPL(DependentCoawaitExpr)
  NO_IMPL(DependentScopeDeclRefExpr)
  NO_IMPL(DesignatedInitExpr)
  NO_IMPL(DesignatedInitUpdateExpr)
  NO_IMPL(ExpressionTraitExpr)
  NO_IMPL(ExtVectorElementExpr)
  DECLARE_VISITOR(FixedPointLiteral)
  DECLARE_VISITOR(FloatingLiteral)
  NO_IMPL(FullExpr)
  NO_IMPL(ConstantExpr)
  NO_IMPL(ExprWithCleanups)
  NO_IMPL(FunctionParmPackExpr)
  NO_IMPL(GNUNullExpr)
  NO_IMPL(GenericSelectionExpr)
  NO_IMPL(ImaginaryLiteral)
  NO_IMPL(ImplicitValueInitExpr)
  NO_IMPL(InitListExpr)
  DECLARE_VISITOR(IntegerLiteral)
  NO_IMPL(LambdaExpr)
  NO_IMPL(MSPropertyRefExpr)
  NO_IMPL(MSPropertySubscriptExpr)
  NO_IMPL(MaterializeTemporaryExpr)
  DECLARE_VISITOR(MemberExpr)
  NO_IMPL(NoInitExpr)
  NO_IMPL(OMPArraySectionExpr)
  NO_IMPL(ObjCArrayLiteral)
  NO_IMPL(ObjCAvailabilityCheckExpr)
  NO_IMPL(ObjCBoolLiteralExpr)
  NO_IMPL(ObjCBoxedExpr)
  NO_IMPL(ObjCDictionaryLiteral)
  NO_IMPL(ObjCEncodeExpr)
  NO_IMPL(ObjCIndirectCopyRestoreExpr)
  NO_IMPL(ObjCIsaExpr)
  NO_IMPL(ObjCIvarRefExpr)
  NO_IMPL(ObjCMessageExpr)
  NO_IMPL(ObjCPropertyRefExpr)
  NO_IMPL(ObjCProtocolExpr)
  NO_IMPL(ObjCSelectorExpr)
  NO_IMPL(ObjCStringLiteral)
  NO_IMPL(ObjCSubscriptRefExpr)
  NO_IMPL(OffsetOfExpr)
  NO_IMPL(OpaqueValueExpr)
  DECLARE_VISITOR(OverloadExpr)
  NO_IMPL(UnresolvedLookupExpr)
  NO_IMPL(UnresolvedMemberExpr)
  NO_IMPL(PackExpansionExpr)
  NO_IMPL(ParenExpr)
  NO_IMPL(ParenListExpr)
  NO_IMPL(PredefinedExpr)
  NO_IMPL(PseudoObjectExpr)
  NO_IMPL(RequiresExpr)
  NO_IMPL(ShuffleVectorExpr)
  NO_IMPL(SizeOfPackExpr)
  NO_IMPL(SourceLocExpr)
  NO_IMPL(StmtExpr)
  DECLARE_VISITOR(StringLiteral)
  NO_IMPL(SubstNonTypeTemplateParmExpr)
  NO_IMPL(SubstNonTypeTemplateParmPackExpr)
  NO_IMPL(TypeTraitExpr)
  NO_IMPL(TypoExpr)
  NO_IMPL(UnaryExprOrTypeTraitExpr)
  NO_IMPL(UnaryOperator)
  NO_IMPL(VAArgExpr)
  NO_IMPL(LabelStmt)
  NO_IMPL(WhileStmt)

  // Comments.
  const char *getCommandName(unsigned CommandID);
  void dumpComment(const Comment *C);

  // Inline comments.
  DECLARE_LOWERCASE_VISITOR(Comment)
  NO_LOWERCASE_IMPL(InlineCommandComment)
  NO_LOWERCASE_IMPL(HTMLStartTagComment)
  NO_LOWERCASE_IMPL(HTMLEndTagComment)
  NO_LOWERCASE_IMPL(BlockCommandComment)
  NO_LOWERCASE_IMPL(ParamCommandComment)
  NO_LOWERCASE_IMPL(TParamCommandComment)
  NO_LOWERCASE_IMPL(VerbatimBlockComment)
  NO_LOWERCASE_IMPL(VerbatimBlockLineComment)
  NO_LOWERCASE_IMPL(VerbatimLineComment)
  NO_LOWERCASE_IMPL(TextComment)

  // Types - no template type handling yet
  int TypeWithChildInfoTupleSize();

  DECLARE_VISITOR(Type)
  DECLARE_VISITOR(AdjustedType)
  //NO_IMPL(DecayedType)
  DECLARE_VISITOR(ArrayType)
  DECLARE_VISITOR(ConstantArrayType)
  //NO_IMPL(DependentSizedArrayType)
  //NO_IMPL(IncompleteArrayType)
  DECLARE_VISITOR(VariableArrayType)
  DECLARE_VISITOR(AtomicType)
  DECLARE_VISITOR(AttributedType)
  DECLARE_VISITOR(BlockPointerType)
  DECLARE_VISITOR(BuiltinType)
  //NO_IMPL(ComplexType)
  DECLARE_VISITOR(DecltypeType)
  //NO_IMPL(DeducedType)
  //NO_IMPL(AutoType)
  //NO_IMPL(DeducedTemplateSpecializationType)
  //NO_IMPL(DependentAddressSpaceType)
  DECLARE_VISITOR(DependentNameType)
  //NO_IMPL(DependentSizedExtVectorType)
  //NO_IMPL(DependentTemplateSpecializationType)
  //NO_IMPL(DependentVectorType)
  //NO_IMPL(ElaboratedType)
  DECLARE_VISITOR(FunctionType)
  //NO_IMPL(FunctionNoProtoType)
  DECLARE_VISITOR(FunctionProtoType)
  DECLARE_VISITOR(InjectedClassNameType)
  //NO_IMPL(MacroQualifiedType)
  DECLARE_VISITOR(MemberPointerType)
  //NO_IMPL(ObjCObjectPointerType)
  //NO_IMPL(ObjCObjectType)
  //NO_IMPL(ObjCInterfaceType)
  //NO_IMPL(ObjCTypeParamType)
  //NO_IMPL(PackExpansionType)
  DECLARE_VISITOR(ParenType)
  //NO_IMPL(PipeType)
  DECLARE_VISITOR(PointerType)
  DECLARE_VISITOR(ReferenceType)
  //NO_IMPL(LValueReferenceType)
  //NO_IMPL(RValueReferenceType)
  DECLARE_VISITOR(SubstTemplateTypeParmType)
  DECLARE_VISITOR(TagType)
  //NO_IMPL(EnumType)
  //NO_IMPL(RecordType)
  DECLARE_VISITOR(TemplateSpecializationType)
  DECLARE_VISITOR(TemplateTypeParmType)
  //NO_IMPL(TypeOfExprType)
  //NO_IMPL(TypeOfType)
  DECLARE_VISITOR(TypedefType)
  //NO_IMPL(UnaryTransformType)
  //NO_IMPL(UnresolvedUsingType)
  //NO_IMPL(VectorType)
  //NO_IMPL(ExtVectorType)

  void dumpAttrKind(attr::Kind Kind);
  void dumpAttr(const Attr *A);

  DECLARE_VISITOR(Attr)
  NO_IMPL(AddressSpaceAttr)
  NO_IMPL(NoDerefAttr)
  NO_IMPL(ObjCGCAttr)
  NO_IMPL(ObjCInertUnsafeUnretainedAttr)
  NO_IMPL(ObjCKindOfAttr)
  NO_IMPL(OpenCLConstantAddressSpaceAttr)
  NO_IMPL(OpenCLGenericAddressSpaceAttr)
  NO_IMPL(OpenCLGlobalAddressSpaceAttr)
  NO_IMPL(OpenCLLocalAddressSpaceAttr)
  NO_IMPL(OpenCLPrivateAddressSpaceAttr)
  NO_IMPL(Ptr32Attr)
  NO_IMPL(Ptr64Attr)
  NO_IMPL(SPtrAttr)
  NO_IMPL(TypeNonNullAttr)
  NO_IMPL(TypeNullUnspecifiedAttr)
  NO_IMPL(TypeNullableAttr)
  NO_IMPL(UPtrAttr)
  NO_IMPL(FallThroughAttr)
  NO_IMPL(SuppressAttr)
  NO_IMPL(AArch64VectorPcsAttr)
  NO_IMPL(AcquireHandleAttr)
  NO_IMPL(AnyX86NoCfCheckAttr)
  NO_IMPL(CDeclAttr)
  NO_IMPL(FastCallAttr)
  NO_IMPL(IntelOclBiccAttr)
  NO_IMPL(LifetimeBoundAttr)
  NO_IMPL(MSABIAttr)
  NO_IMPL(NSReturnsRetainedAttr)
  NO_IMPL(ObjCOwnershipAttr)
  NO_IMPL(PascalAttr)
  NO_IMPL(PcsAttr)
  NO_IMPL(PreserveAllAttr)
  NO_IMPL(RegCallAttr)
  NO_IMPL(StdCallAttr)
  NO_IMPL(SwiftCallAttr)
  NO_IMPL(SysVABIAttr)
  NO_IMPL(ThisCallAttr)
  NO_IMPL(VectorCallAttr)
  NO_IMPL(SwiftContextAttr)
  NO_IMPL(SwiftErrorResultAttr)
  NO_IMPL(SwiftIndirectResultAttr)
  DECLARE_VISITOR(AnnotateAttr)
  NO_IMPL(CFConsumedAttr)
  NO_IMPL(CarriesDependencyAttr)
  NO_IMPL(NSConsumedAttr)
  NO_IMPL(NonNullAttr)
  NO_IMPL(OSConsumedAttr)
  NO_IMPL(PassObjectSizeAttr)
  NO_IMPL(ReleaseHandleAttr)
  NO_IMPL(UseHandleAttr)
  NO_IMPL(AMDGPUFlatWorkGroupSizeAttr)
  NO_IMPL(AMDGPUNumSGPRAttr)
  NO_IMPL(AMDGPUNumVGPRAttr)
  NO_IMPL(AMDGPUWavesPerEUAttr)
  NO_IMPL(ARMInterruptAttr)
  NO_IMPL(AVRInterruptAttr)
  NO_IMPL(AVRSignalAttr)
  NO_IMPL(AcquireCapabilityAttr)
  NO_IMPL(AcquiredAfterAttr)
  NO_IMPL(AlignMac68kAttr)
  NO_IMPL(AlignedAttr)
  NO_IMPL(AllocAlignAttr)
  NO_IMPL(AllocSizeAttr)
  NO_IMPL(AlwaysDestroyAttr)
  NO_IMPL(AlwaysInlineAttr)
  NO_IMPL(AnalyzerNoReturnAttr)
  NO_IMPL(AnyX86InterruptAttr)
  NO_IMPL(AnyX86NoCallerSavedRegistersAttr)
  NO_IMPL(ArcWeakrefUnavailableAttr)
  NO_IMPL(ArgumentWithTypeTagAttr)
  NO_IMPL(ArmMveAliasAttr)
  NO_IMPL(ArtificialAttr)
  NO_IMPL(AsmLabelAttr)
  NO_IMPL(AssertCapabilityAttr)
  NO_IMPL(AssertExclusiveLockAttr)
  NO_IMPL(AssertSharedLockAttr)
  NO_IMPL(AssumeAlignedAttr)
  DECLARE_VISITOR(AvailabilityAttr)
  NO_IMPL(BPFPreserveAccessIndexAttr)
  NO_IMPL(BlocksAttr)
  NO_IMPL(C11NoReturnAttr)
  NO_IMPL(CFAuditedTransferAttr)
  NO_IMPL(CFGuardAttr)
  NO_IMPL(CFICanonicalJumpTableAttr)
  NO_IMPL(CFReturnsNotRetainedAttr)
  NO_IMPL(CFReturnsRetainedAttr)
  NO_IMPL(CFUnknownTransferAttr)
  NO_IMPL(CPUDispatchAttr)
  NO_IMPL(CPUSpecificAttr)
  NO_IMPL(CUDAConstantAttr)
  NO_IMPL(CUDADeviceAttr)
  NO_IMPL(CUDAGlobalAttr)
  NO_IMPL(CUDAHostAttr)
  NO_IMPL(CUDAInvalidTargetAttr)
  NO_IMPL(CUDALaunchBoundsAttr)
  NO_IMPL(CUDASharedAttr)
  NO_IMPL(CXX11NoReturnAttr)
  NO_IMPL(CallableWhenAttr)
  NO_IMPL(CallbackAttr)
  NO_IMPL(CapabilityAttr)
  NO_IMPL(CapturedRecordAttr)
  NO_IMPL(CleanupAttr)
  NO_IMPL(CodeSegAttr)
  NO_IMPL(ColdAttr)
  NO_IMPL(CommonAttr)
  NO_IMPL(ConstAttr)
  NO_IMPL(ConstInitAttr)
  NO_IMPL(ConstructorAttr)
  NO_IMPL(ConsumableAttr)
  NO_IMPL(ConsumableAutoCastAttr)
  NO_IMPL(ConsumableSetOnReadAttr)
  NO_IMPL(ConvergentAttr)
  NO_IMPL(DLLExportAttr)
  NO_IMPL(DLLExportStaticLocalAttr)
  NO_IMPL(DLLImportAttr)
  NO_IMPL(DLLImportStaticLocalAttr)
  NO_IMPL(DeprecatedAttr)
  NO_IMPL(DestructorAttr)
  NO_IMPL(DiagnoseIfAttr)
  NO_IMPL(DisableTailCallsAttr)
  NO_IMPL(EmptyBasesAttr)
  NO_IMPL(EnableIfAttr)
  NO_IMPL(EnumExtensibilityAttr)
  NO_IMPL(ExcludeFromExplicitInstantiationAttr)
  NO_IMPL(ExclusiveTrylockFunctionAttr)
  NO_IMPL(ExternalSourceSymbolAttr)
  NO_IMPL(FinalAttr)
  NO_IMPL(FlagEnumAttr)
  NO_IMPL(FlattenAttr)
  NO_IMPL(FormatAttr)
  NO_IMPL(FormatArgAttr)
  NO_IMPL(GNUInlineAttr)
  NO_IMPL(GuardedByAttr)
  NO_IMPL(GuardedVarAttr)
  NO_IMPL(HIPPinnedShadowAttr)
  NO_IMPL(HotAttr)
  NO_IMPL(IBActionAttr)
  NO_IMPL(IBOutletAttr)
  NO_IMPL(IBOutletCollectionAttr)
  NO_IMPL(InitPriorityAttr)
  NO_IMPL(InternalLinkageAttr)
  NO_IMPL(LTOVisibilityPublicAttr)
  NO_IMPL(LayoutVersionAttr)
  NO_IMPL(LockReturnedAttr)
  NO_IMPL(LocksExcludedAttr)
  NO_IMPL(MIGServerRoutineAttr)
  NO_IMPL(MSAllocatorAttr)
  NO_IMPL(MSInheritanceAttr)
  NO_IMPL(MSNoVTableAttr)
  NO_IMPL(MSP430InterruptAttr)
  NO_IMPL(MSStructAttr)
  NO_IMPL(MSVtorDispAttr)
  NO_IMPL(MaxFieldAlignmentAttr)
  NO_IMPL(MayAliasAttr)
  NO_IMPL(MicroMipsAttr)
  NO_IMPL(MinSizeAttr)
  NO_IMPL(MinVectorWidthAttr)
  NO_IMPL(Mips16Attr)
  NO_IMPL(MipsInterruptAttr)
  NO_IMPL(MipsLongCallAttr)
  NO_IMPL(MipsShortCallAttr)
  NO_IMPL(NSConsumesSelfAttr)
  NO_IMPL(NSReturnsAutoreleasedAttr)
  NO_IMPL(NSReturnsNotRetainedAttr)
  NO_IMPL(NakedAttr)
  NO_IMPL(NoAliasAttr)
  NO_IMPL(NoCommonAttr)
  NO_IMPL(NoDebugAttr)
  NO_IMPL(NoDestroyAttr)
  NO_IMPL(NoDuplicateAttr)
  NO_IMPL(NoInlineAttr)
  NO_IMPL(NoInstrumentFunctionAttr)
  NO_IMPL(NoMicroMipsAttr)
  NO_IMPL(NoMips16Attr)
  NO_IMPL(NoReturnAttr)
  NO_IMPL(NoSanitizeAttr)
  NO_IMPL(NoSpeculativeLoadHardeningAttr)
  NO_IMPL(NoSplitStackAttr)
  NO_IMPL(NoStackProtectorAttr)
  NO_IMPL(NoThreadSafetyAnalysisAttr)
  NO_IMPL(NoThrowAttr)
  NO_IMPL(NoUniqueAddressAttr)
  NO_IMPL(NotTailCalledAttr)
  NO_IMPL(OMPAllocateDeclAttr)
  NO_IMPL(OMPCaptureNoInitAttr)
  NO_IMPL(OMPDeclareTargetDeclAttr)
  NO_IMPL(OMPDeclareVariantAttr)
  NO_IMPL(OMPThreadPrivateDeclAttr)
  NO_IMPL(OSConsumesThisAttr)
  NO_IMPL(OSReturnsNotRetainedAttr)
  NO_IMPL(OSReturnsRetainedAttr)
  NO_IMPL(OSReturnsRetainedOnNonZeroAttr)
  NO_IMPL(OSReturnsRetainedOnZeroAttr)
  NO_IMPL(ObjCBridgeAttr)
  NO_IMPL(ObjCBridgeMutableAttr)
  NO_IMPL(ObjCBridgeRelatedAttr)
  NO_IMPL(ObjCExceptionAttr)
  NO_IMPL(ObjCExplicitProtocolImplAttr)
  NO_IMPL(ObjCExternallyRetainedAttr)
  NO_IMPL(ObjCIndependentClassAttr)
  NO_IMPL(ObjCMethodFamilyAttr)
  NO_IMPL(ObjCNSObjectAttr)
  NO_IMPL(ObjCPreciseLifetimeAttr)
  NO_IMPL(ObjCRequiresPropertyDefsAttr)
  NO_IMPL(ObjCRequiresSuperAttr)
  NO_IMPL(ObjCReturnsInnerPointerAttr)
  NO_IMPL(ObjCRootClassAttr)
  NO_IMPL(ObjCSubclassingRestrictedAttr)
  NO_IMPL(OpenCLIntelReqdSubGroupSizeAttr)
  NO_IMPL(OpenCLKernelAttr)
  NO_IMPL(OpenCLUnrollHintAttr)
  NO_IMPL(OptimizeNoneAttr)
  NO_IMPL(OverrideAttr)
  NO_IMPL(OwnerAttr)
  NO_IMPL(OwnershipAttr)
  NO_IMPL(PackedAttr)
  NO_IMPL(ParamTypestateAttr)
  NO_IMPL(PatchableFunctionEntryAttr)
  NO_IMPL(PointerAttr)
  NO_IMPL(PragmaClangBSSSectionAttr)
  NO_IMPL(PragmaClangDataSectionAttr)
  NO_IMPL(PragmaClangRelroSectionAttr)
  NO_IMPL(PragmaClangRodataSectionAttr)
  NO_IMPL(PragmaClangTextSectionAttr)
  NO_IMPL(PtGuardedByAttr)
  NO_IMPL(PtGuardedVarAttr)
  NO_IMPL(PureAttr)
  NO_IMPL(RISCVInterruptAttr)
  NO_IMPL(ReinitializesAttr)
  NO_IMPL(ReqdWorkGroupSizeAttr)
  NO_IMPL(ReleaseCapabilityAttr)
  NO_IMPL(RestrictAttr)
  NO_IMPL(ReturnTypestateAttr)
  NO_IMPL(ReturnsNonNullAttr)
  NO_IMPL(ReturnsTwiceAttr)
  NO_IMPL(SYCLKernelAttr)
  NO_IMPL(ScopedLockableAttr)
  NO_IMPL(SectionAttr)
  NO_IMPL(SelectAnyAttr)
  DECLARE_VISITOR(SentinelAttr)
  NO_IMPL(SetTypestateAttr)
  NO_IMPL(SharedTrylockFunctionAttr)
  NO_IMPL(SpeculativeLoadHardeningAttr)
  NO_IMPL(TLSModelAttr)
  NO_IMPL(TargetAttr)
  NO_IMPL(TestTypestateAttr)
  NO_IMPL(TransparentUnionAttr)
  NO_IMPL(TypeTagForDatatypeAttr)
  NO_IMPL(TrivialABIAttr)
  NO_IMPL(TryAcquireCapabilityAttr)
  NO_IMPL(TypeVisibilityAttr)
  NO_IMPL(UnavailableAttr)
  NO_IMPL(UninitializedAttr)
  NO_IMPL(UnusedAttr)
  NO_IMPL(UsedAttr)
  NO_IMPL(UuidAttr)
  NO_IMPL(VecReturnAttr)
  DECLARE_VISITOR(VisibilityAttr)
  NO_IMPL(WarnUnusedAttr)
  NO_IMPL(WarnUnusedResultAttr)
  NO_IMPL(WeakAttr)
  NO_IMPL(WeakImportAttr)
  NO_IMPL(WeakRefAttr)
  NO_IMPL(WebAssemblyExportNameAttr)
  NO_IMPL(WebAssemblyImportModuleAttr)
  NO_IMPL(WebAssemblyImportNameAttr)
  NO_IMPL(WorkGroupSizeHintAttr)
  NO_IMPL(X86ForceAlignArgPointerAttr)
  NO_IMPL(XRayInstrumentAttr)
  NO_IMPL(XRayLogArgsAttr)
  NO_IMPL(AbiTagAttr)
  NO_IMPL(AliasAttr)
  NO_IMPL(AlignValueAttr)
  NO_IMPL(IFuncAttr)
  NO_IMPL(InitSegAttr)
  NO_IMPL(LoopHintAttr)
  NO_IMPL(ModeAttr)
  NO_IMPL(NoBuiltinAttr)
  NO_IMPL(NoEscapeAttr)
  NO_IMPL(OMPCaptureKindAttr)
  NO_IMPL(OMPDeclareSimdDeclAttr)
  NO_IMPL(OMPReferencedVarAttr)
  NO_IMPL(ObjCBoxableAttr)
  NO_IMPL(ObjCClassStubAttr)
  NO_IMPL(ObjCDesignatedInitializerAttr)
  NO_IMPL(ObjCDirectAttr)
  NO_IMPL(ObjCDirectMembersAttr)
  NO_IMPL(ObjCNonLazyClassAttr)
  NO_IMPL(ObjCRuntimeNameAttr)
  NO_IMPL(ObjCRuntimeVisibleAttr)
  NO_IMPL(OpenCLAccessAttr)
  NO_IMPL(OverloadableAttr)
  NO_IMPL(RenderScriptKernelAttr)
  NO_IMPL(ThreadAttr)
  NO_IMPL(TypeAttr)
  NO_IMPL(StmtAttr)
  NO_IMPL(InheritableAttr)
  NO_IMPL(InheritableParamAttr)
  NO_IMPL(ParameterABIAttr)

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

template <class ATDWriter>
bool ASTExporter<ATDWriter>::inMainFile(Decl const *D) {
    if (!D) { return false; }
    SourceManager &SM = Context.getSourceManager();
    SourceLocation loc = D->getLocation();
    return SM.isInMainFile(loc);
}

template <class ATDWriter>
bool ASTExporter<ATDWriter>::declWritten(Decl const *D) {
    for (auto &dptr : decls_written) {
        if (dptr == D) { return true; }
    }
    return false;
}

template <class ATDWriter>
bool ASTExporter<ATDWriter>::typeWritten(Type const *T) {
    for (auto &tptr : types_written) {
        if (tptr == T) { return true; }
    }
    return false;
}

template <class ATDWriter>
bool ASTExporter<ATDWriter>::declIsReferenced(Decl const *D) {
    for (auto &decl_refd : decls_referenced) {
        if (D == decl_refd) {
            return true;
        }
    }
    return false;
}

template <class ATDWriter>
bool ASTExporter<ATDWriter>::declIsInherited(Decl const *D) {
    for (auto &decl_inherited : decls_inherited) {
        if (D == decl_inherited) {
            return true;
        }
    }
    return false;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::referenceDecl(Decl const *D) {

    TranslationUnitDecl const *TU = 
        dyn_cast<TranslationUnitDecl>(D);
    NamespaceDecl const *ND =
        dyn_cast<NamespaceDecl>(D);

    bool isTU = static_cast<bool>(TU);
    bool isNS = static_cast<bool>(ND);

    if (!D || isTU || isNS) {
        return;
    }

    decls_referenced.push_back(D);
    new_decls_referenced.push_back(D);
    return;
}

std::unordered_map<const void *, int> pointerMap;
int pointerCounter = 1;

template <class ATDWriter>
void ASTExporter<ATDWriter>::writePointer(bool withPointers, const void *Ptr) {
  if (!Ptr) {
    OF.emitInteger(0);
    return;
  }
  if (pointerMap.find(Ptr) == pointerMap.end()) {
    pointerMap[Ptr] = pointerCounter++;
  }

  if (withPointers) {
    OF.emitInteger(pointerMap[Ptr]);
  }

  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpDeclPointer(Decl const *Ptr) {
  if (!inMainFile(Ptr)) {
      bool alreadyReferenced = false;
      for (auto &decl : decls_referenced) {
          if (decl == Ptr) {
              alreadyReferenced = true;
          }
      }
      if (!alreadyReferenced) {
          referenceDecl(Ptr);
      }
  }
  dumpPointer(Ptr);
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

  OF.emitTag("file");
  dumpSourceFile(R.getBegin());

  OF.emitTag("begin");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceLocation(R.getBegin());
  }

  OF.emitTag("end");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceLocation(R.getEnd());
  }

  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpQualType(const QualType &qt) {

  bool isNull = qt.isNull();
  clang::Qualifiers Quals = isNull ? clang::Qualifiers() : qt.getQualifiers();
  bool isConst = Quals.hasConst();
  bool isRestrict = Quals.hasRestrict();
  bool isVolatile = Quals.hasVolatile();

  OF.emitTag("type_pointer");
  dumpQualTypeNoQuals(qt);

  PrintingPolicy pp(Context.getLangOpts());

  OF.emitTag("type");
  OF.emitString(qt.getAsString(pp));

  OF.emitTag("canonical");
  if (!isNull) {
    QualType canonical = qt.getCanonicalType();
    OF.emitString(canonical.getAsString(pp));
  } else {
    OF.emitString("None");
  }

  // OF.emitString(canonical.getAsString(pp));

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
void ASTExporter<ATDWriter>::dumpDeclRef(const Decl &D, bool reference) {

  const NamedDecl *ND = dyn_cast<NamedDecl>(&D);
  const ValueDecl *VD = dyn_cast<ValueDecl>(&D);
  bool IsHidden = ND && ND->isHidden();

  OF.emitTag("kind");
  OF.emitString(D.getDeclKindName());

  OF.emitTag("skipped");

  if (ND && !NamePrint.goodDeclName(ND)) {
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("bad name");
      return;
  }

  if (IsHidden) {
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("decl hidden");
      return;
  }

  if (VD && typeIsHidden(VD->getType())) {
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitBoolean("type hidden");
      return;
  }

  OF.emitBoolean(false);
  
  OF.emitTag("decl_pointer");
  if (reference) {
    dumpDeclPointer(&D);
  } else {
    dumpPointer(0);
  }

  OF.emitTag("id");
  if (ND) {
    ObjectScope oScope(OF, 1);
    dumpName(*ND);
  } else {
    OF.emitString("None");
  }

  OF.emitTag("is_hidden");
  OF.emitBoolean(IsHidden);

  OF.emitTag("qual_type");
  if (VD) {
    ObjectScope oScope(OF, 1);
    dumpQualType(VD->getType());
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::DeclContextTupleSize() {
  return 2;
}

template <class ATDWriter>
bool ASTExporter<ATDWriter>::declIsHidden(const Decl *D) {
  NamedDecl const *ND = dyn_cast<NamedDecl>(D);
  if (static_cast<bool>(ND)) {
      std::string name = ND->getNameAsString();
      return (!name.rfind("__", 0) ||
              !name.rfind("typename __", 0));
  }
  return false;
}

template <class ATDWriter>
bool ASTExporter<ATDWriter>::typeIsHidden(const QualType &T) {
    std::string name = T.getAsString();
    return (!name.rfind("__", 0) ||
            !name.rfind("typename __", 0));
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::contextCanBlock(const DeclContext *DC) {
    int ret = 0;
    bool isTranslationUnit = DC->isTranslationUnit();
    Decl const *D = dyn_cast<Decl>(DC);
    ret = static_cast<int>(!isTranslationUnit);
    if (isa<NamedDecl>(DC)) {
        ret = declIsHidden(D) ? 2 : ret;
    }
    return ret;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::declCanWrite(const Decl *D) {
    int ret = 0;
    NamedDecl const *ND = dyn_cast<NamedDecl>(D);
    ret = parsing_refs ? 1 : static_cast<int>(inMainFile(D));
    if (static_cast<bool>(ND)) {
        ret = declIsHidden(D) ? -1 : ret;
    }
    if (parsing_refs) {
        if (Options.recursionLevel == 0) {
            ret = -2;
        } else if (Options.recursionLevel == 1 && !declIsInherited(D)) {
            ret = -2;
        }
    }
    return ret;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitDeclContext(const DeclContext *DC) {

  OF.emitTag("skipped");
  int block_ret = contextCanBlock(DC);
  int write_ret = declCanWrite(dyn_cast<Decl>(DC));
  if (block_ret) {
      if (write_ret <= 0) {
          OF.emitBoolean(true);
          std::string reason;

          if (block_ret == 1) {
              reason = "Context is not a translation unit and ";
          } else if (block_ret == 2) {
              reason = "Context name is hidden and ";
          }


          if (write_ret == 0) {
              reason += "decl is not in the main file";
          } else if (write_ret == -1) {
              reason += "decl name is hidden";
          } else if (write_ret == -2) {
              reason += "recursion is not allowed";
          }

          OF.emitTag("reason");
          OF.emitString(reason.c_str());

          bool isNamed = isa<NamedDecl>(DC);
          if (isNamed) {
              OF.emitTag("id");
              ObjectScope oScope(OF, 2);
              dumpName(*cast<NamedDecl>(DC));
          }

          OF.emitTag("pointer");
          dumpPointer(DC);
          return;

      }
  }
  OF.emitBoolean(false);

  if (!DC) {

    OF.emitTag("c_linkage");
    OF.emitString("Unknown");

    OF.emitTag("has_external_lexical_storage");
    OF.emitString("Unknown");

    OF.emitTag("has_external_visible_storage");
    OF.emitString("Unknown");

    OF.emitTag("declarations");
    { ArrayScope aScope(OF, 0); }

    return;
  }

  OF.emitTag("c_linkage");
  OF.emitBoolean(DC->isExternCContext());

  OF.emitTag("has_external_lexical_storage");
  OF.emitBoolean(DC->hasExternalLexicalStorage());

  OF.emitTag("has_external_visible_storage");
  OF.emitBoolean(DC->hasExternalVisibleStorage());

  std::vector<Decl const *> declsToDump;
  for (auto I : DC->decls()) {
    declsToDump.push_back(I);
  }

  OF.emitTag("declarations");

  ArrayScope aScope(OF, declsToDump.size());
  for (auto I : declsToDump) {
    if (!parsing_refs && !inMainFile(I)) {
        continue;
    }
    dumpDecl(I);
  }

  if (!OF.block()) {
    decls_written.push_back(cast<Decl>(DC));
  }

  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpLookups(const DeclContext &DC) {

  bool isNamed = isa<NamedDecl>(DC);
  OF.emitTag("decl_ref");
  if (isNamed && NamePrint.goodDeclName(cast<NamedDecl>(DC))) {
    dumpDeclRef(cast<Decl>(DC));
  } else {
    OF.emitString("None");
  }

  const DeclContext *Primary = DC.getPrimaryContext();
  isNamed = isa<NamedDecl>(DC);
  OF.emitTag("primary_context");
  if (isNamed && NamePrint.goodDeclName(cast<NamedDecl>(Primary))) {
    dumpDeclPointer(cast<Decl>(Primary));
  } else if (isNamed) {
      dumpName(Primary);
  } else {
    OF.emitString("None");
  }

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
          isNamed = isa<NamedDecl>(*RI);
          if (isNamed && !NamePrint.goodDeclName(cast<NamedDecl>(*RI))) {
              continue;
          }
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
  OF.emitTag("access_specifier");
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

  const FieldDecl *FD = Init.getAnyMember();
  bool isBaseClass = false;

  OF.emitTag("kind");
  if (FD) {

    OF.emitString("member");

    OF.emitTag("declaration");
    {
      ObjectScope oScope(OF, 1);
      dumpDeclRef(*FD);
    }

    OF.emitTag("qualified_type");
    OF.emitString("None");

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
    OF.emitString("None");

    OF.emitTag("qualified_type");
    dumpQualTypeNoQuals(Init.getTypeSourceInfo()->getType());

    OF.emitTag("virtual_base");
    OF.emitBoolean(Init.isBaseVirtual());
  }

  OF.emitTag("location");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceRange(Init.getSourceRange());
  }

  OF.emitTag("init_expr");
  if (E) {
    dumpStmt(E);
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpDeclarationName(const DeclarationName &Name) {

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
      { OF.emitString("None"); }
      break;
    case NestedNameSpecifier::Namespace:
      OF.emitString("Namespace");
      OF.emitTag("ref");
      {
        ObjectScope oScope(OF, 1);
        NamespaceDecl const *ND = NNS.getNestedNameSpecifier()->getAsNamespace();
        if (NamePrint.goodDeclName(ND)) {
            dumpDeclRef(
                    *NNS.getNestedNameSpecifier()->getAsNamespace(),
                    inMainFile(NNS.getNestedNameSpecifier()->getAsNamespace()));
        } else {
            dumpName(*ND);
        }
      }
      break;
    case NestedNameSpecifier::NamespaceAlias:
      OF.emitString("NamespaceAlias");
      OF.emitTag("ref");
      {
        ObjectScope oScope(OF, 1);
        NamespaceDecl const *ND = NNS.getNestedNameSpecifier()->getAsNamespace();
        if (NamePrint.goodDeclName(ND)) {
            dumpDeclRef(
                *ND,
                inMainFile(ND));
        } else {
            dumpName(*ND);
        }
      }

      break;
    case NestedNameSpecifier::TypeSpec:
      OF.emitString("TypeSpec");
      OF.emitTag("ref");
      { OF.emitString("None"); }
      break;
    case NestedNameSpecifier::TypeSpecWithTemplate:
      OF.emitString("TypeSpecWithTemplate");
      OF.emitTag("ref");
      { OF.emitString("None"); }
      break;
    case NestedNameSpecifier::Global:
      OF.emitString("Global");
      OF.emitTag("ref");
      { OF.emitString("None"); }
      break;
    case NestedNameSpecifier::Super:
      OF.emitString("Super");
      OF.emitTag("ref");
      { OF.emitString("None"); }
      break;
    }
  }
  return;
}

template <class ATDWriter>
bool ASTExporter<ATDWriter>::alwaysEmitParent(const Decl *D) {
  if (isa<ObjCMethodDecl>(D) || isa<CXXMethodDecl>(D) || isa<FieldDecl>(D) ||
      isa<ObjCIvarDecl>(D) || isa<BlockDecl>(D) || isa<ObjCInterfaceDecl>(D) ||
      isa<ObjCImplementationDecl>(D) || isa<ObjCCategoryDecl>(D) ||
      isa<ObjCCategoryImplDecl>(D) || isa<ObjCPropertyDecl>(D) ||
      isa<RecordDecl>(D) || isa<ObjCProtocolDecl>(D)) {
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
void ASTExporter<ATDWriter>::dumpDecl(const Decl *D, bool force) {

  if (!force && !parsing_refs && !inMainFile(D)) {
      return;
  }

  ObjectScope oScope(OF, 4);
  OF.emitTag("clang_kind");
  OF.emitString("Decl");

  std::string declKind = D->getDeclKindName() + std::string("Decl");
  OF.emitTag("kind");
  OF.emitString(declKind);

  OF.emitTag("skipped");

  int write_ret = declCanWrite(D);
  if (write_ret <= 0 && !force) {
      OF.emitBoolean(true);
      OF.emitTag("reason");
      if (write_ret == 0) {
          OF.emitString("decl not in main file");
      } else if (write_ret == -1) {
          OF.emitString("decl is hidden");
      } if (write_ret == -2) {
          OF.emitString("recursion is not allowed");
      }

      bool isNamed = isa<NamedDecl>(D);
      if (isNamed) {
          OF.emitTag("id");

          ObjectScope oScope(OF, 2);
          dumpName(*cast<NamedDecl>(D));
      }

      OF.emitTag("pointer");
      dumpPointer(D);

      return;
  }
  OF.emitBoolean(false);
  
  OF.emitTag("content");
  if (!D) {
    // We use a fixed EmptyDecl node to represent null pointers
    D = NullPtrDecl;
    OF.emitString("None");
  }
  {
    ObjectScope oScope(OF, 1);
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

    OF.emitTag("pointer");
    dumpDeclPointer(D);

    OF.emitTag("parent_pointer");
    if (ShouldEmitParentPointer) {
      dumpDeclPointer(cast<Decl>(D->getDeclContext()));
    } else {
      OF.emitString("None");
    }

    OF.emitTag("location");
    {
      ObjectScope oScope(OF, 1);
      dumpSourceRange(D->getSourceRange());
    }

    OF.emitTag("owning_module");
    if (M) {
      OF.emitString(M->getFullModuleName());
    } else {
      OF.emitString("None");
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
        ObjectScope oScope(OF, 1);
        dumpAttr(I);
      }
    } else {
      ArrayScope aScope(OF, 0);
    }

    OF.emitTag("full_comment");
    if (Comment) {
      ObjectScope oScope(OF, 1);
      dumpFullComment(Comment);
    } else {
      OF.emitString("None");
    }

    if (HasAccess) {
      dumpAccessSpecifier(Access);
    } else {
      OF.emitTag("access_specifier");
      OF.emitString("None");
    }
  }

  if (!OF.block()) {
    decls_written.push_back(D);
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CapturedDeclTupleSize() {
  return DeclTupleSize() + DeclContextTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCapturedDecl(const CapturedDecl *D) {

  OF.emitTag("decl");
  {
    ObjectScope oScope(OF, 1);
    VisitDecl(D);
  }

  OF.emitTag("context");
  {
    ObjectScope oScope(OF, 1);
    VisitDeclContext(D);
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::LinkageSpecDeclTupleSize() {
  return DeclTupleSize() + DeclContextTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitLinkageSpecDecl(const LinkageSpecDecl *D) {

  OF.emitTag("decl");
  {
    ObjectScope oScope(OF, 1);
    VisitDecl(D);
  }

  OF.emitTag("context");
  {
    ObjectScope oScope(OF, 1);
    VisitDeclContext(D);
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::NamespaceDeclTupleSize() {
  return NamedDeclTupleSize() + DeclContextTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitNamespaceDecl(const NamespaceDecl *D) {

  OF.emitTag("named_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitNamedDecl(D);
  }

  OF.emitTag("context");
  {
    ObjectScope oScope(OF, 1);
    VisitDeclContext(D);
  }

  bool IsInline = D->isInline();
  bool IsOriginalNamespace = D->isOriginalNamespace();

  OF.emitTag("is_inline");
  OF.emitBoolean(IsInline);

  OF.emitTag("original_namespace");
  if (!IsOriginalNamespace) {
    ObjectScope oScope(OF, 1);
    NamedDecl const *ND = cast<NamedDecl>(D);
    if (NamePrint.goodDeclName(ND)) {
        dumpDeclRef(
                *D->getOriginalNamespace(),
                inMainFile(D->getOriginalNamespace()));
    } else {
        dumpName(*ND);
    }
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TagDeclTupleSize() {
  return TypeDeclTupleSize() + DeclContextTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTagDecl(const TagDecl *D) {

  OF.emitTag("type_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitTypeDecl(D);
  }

  OF.emitTag("context");
  {
    ObjectScope oScope(OF, 1);
    VisitDeclContext(D);
  }

  OF.emitTag("tag_kind");
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

  OF.emitTag("named_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitNamedDecl(D);
  }
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

  OF.emitTag("named_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitNamedDecl(D);
  }

  OF.emitTag("qualified_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(D->getType());
  }

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

  OF.emitTag("language_kind");

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

  OF.emitTag("input_path");
  OF.emitString(
      Options.normalizeSourcePath(Options.inputFile.getFile().str().c_str()));

  OF.emitTag("includes");
  {
    ArrayScope aScope(OF);
    for (auto &inc : PreProcessor.includes) {
        ObjectScope oScope(OF);
        OF.emitTag("search_path");
        OF.emitString(inc.first.c_str());
        OF.emitTag("file");
        OF.emitString(inc.second.c_str());
    }
  } 

  OF.emitTag("input_kind");
  {
    ObjectScope oScope(OF, 1);
    dumpInputKind(Options.inputFile.getKind());
  }

  OF.emitTag("integer_type_widths");
  {
    ObjectScope oScope(OF, 1);
    dumpIntegerTypeWidths(Context.getTargetInfo());
  }

  OF.emitTag("main_context");
  {
    // First pass collects only decls in the main file
    ObjectScope oScope(OF, 1);
    VisitDeclContext(D);
  }

  // Do uiet passes over referenced types and decls to collect
  // more decls and references
  parsing_refs = true;
  OF.block(true);
  unsigned int processed_size = 1;
  std::map<const void *, int> processed_map;

  while (processed_map.size() != processed_size) {
    processed_size = processed_map.size();

    std::vector<Decl const *> new_decls_cp = new_decls_referenced;
    std::vector<Type const *> new_types_cp = new_types_referenced;
    new_decls_referenced.clear();
    new_types_referenced.clear();

    // Pick up and reference types that the decls hav referenced
    for (auto &type : new_types_cp) {
        if (!processed_map[type]) {
            dumpType(type);
            processed_map[type] = pointerMap[type];
        }
    }

    for (auto &decl : new_decls_cp) {
        if (!processed_map[decl] && !inMainFile(decl)) {
            dumpDecl(decl);
            processed_map[decl] = pointerMap[decl];
        }
    }
    
  }
  OF.block(false);

  // Collect the referenced decls
  OF.emitTag("referenced_decls");
  {
    ArrayScope aScope(OF, 1);
    for (auto &decl : decls_referenced) {
      if (declWritten(decl)) { continue; }
      bool isNamed = isa<NamedDecl>(decl);
      if (isNamed && !NamePrint.goodDeclName(cast<NamedDecl>(decl))) {
          continue;
      }
      dumpDecl(decl);
    }
  }

  // Do a pass over referenced types
  OF.emitTag("referenced_types");
  ArrayScope aScope(OF, types_referenced.size() + 1); // + 1 for nullptr
  for (auto &type : types_referenced) {
    ObjectScope oScope(OF, 1);
    dumpType(type);
  }

  {
    ObjectScope oScope(OF, 1);
    dumpType(nullptr);
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::NamedDeclTupleSize() {
  return DeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitNamedDecl(const NamedDecl *D) {

  OF.emitTag("decl");
  {
    ObjectScope oScope(OF, 1);
    VisitDecl(D);
  }

  OF.emitTag("id");
  {
    ObjectScope oScope(OF, 1);
    dumpName(*D);
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypedefDeclTupleSize() {
  return ASTExporter::TypedefNameDeclTupleSize() + 2;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTypedefDecl(const TypedefDecl *D) {

  OF.emitTag("type_decl");
  {
    ObjectScope oScope(OF, 1);
    ASTExporter<ATDWriter>::VisitTypeDecl(D);
  }

  bool IsModulePrivate = D->isModulePrivate();

  OF.emitTag("underlying_type");
  {
    QualType const &qt = D->getUnderlyingType();
    if (typeIsHidden(qt)) {
        OF.emitString("None");
    } else {
        ObjectScope oScope(OF, 1);
        dumpQualType(qt);
    }

  }

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

  OF.emitTag("tag_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitTagDecl(D);
  }

  bool IsScoped = D->isScoped();
  bool IsModulePrivate = D->isModulePrivate();

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

  OF.emitTag("tag_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitTagDecl(D);
  }

  bool IsModulePrivate = D->isModulePrivate();
  bool IsCompleteDefinition = D->isCompleteDefinition();
  bool IsDependentType = D->isDependentType();

  OF.emitTag("definition_pointer");
  { dumpDeclPointer(D->getDefinition()); }

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

  OF.emitTag("value_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitValueDecl(D);
  }

  const Expr *Init = D->getInitExpr();

  OF.emitTag("init_expr");
  if (Init) {
    dumpStmt(Init);
  } else {
    OF.emitString("None");
  }

  OF.emitTag("value");
  if (Init) {
    dumpDefaultArgStr(Init);
  } else {
    OF.emitString("None");
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

  OF.emitTag("value_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitValueDecl(D);
  }

  OF.emitTag("decl_refs");
  ArrayScope Scope(
      OF,
      std::distance(D->chain_begin(), D->chain_end())); // not covered by tests
  for (auto I : D->chain()) {
    ObjectScope oScope(OF, 1);
    bool isNamed = isa<NamedDecl>(I);
    if (isNamed && !NamePrint.goodDeclName(cast<NamedDecl>(I))) {
        continue;
    }
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ParmVarDeclTupleSize() {
  return ASTExporter::DeclTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitParmVarDecl(const ParmVarDecl *D) {
  OF.emitTag("value_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitValueDecl(D);
  }

  bool hasDefault = D->hasDefaultArg();
  OF.emitTag("has_default");
  OF.emitBoolean(hasDefault);

  OF.emitTag("default_value");
  if (hasDefault && D->getDefaultArg()) {
    dumpDefaultArgStr(D->getDefaultArg());
  } else {
    OF.emitString("None");
  }

  OF.emitTag("index");
  OF.emitInteger(D->getFunctionScopeIndex());

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FunctionDeclTupleSize() {
  return ASTExporter::DeclaratorDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFunctionDecl(const FunctionDecl *D) {

  OF.emitTag("value_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitValueDecl(D);
  }
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
  FunctionTemplateDecl *TemplateDecl = D->getPrimaryTemplate();

  OF.emitTag("mangled_name");
  if (ShouldMangleName) {
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

  OF.emitTag("is_no_return");
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

  OF.emitTag("template_specialization");
  if (TemplateDecl) {
    ObjectScope oScope(OF, 1);
    dumpTemplateSpecialization(TemplateDecl,
                               *D->getTemplateSpecializationArgs());
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FieldDeclTupleSize() {
  return ASTExporter::DeclaratorDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFieldDecl(const FieldDecl *D) {

  OF.emitTag("value_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitValueDecl(D);
  }

  bool IsMutable = D->isMutable();
  bool IsModulePrivate = D->isModulePrivate();
  bool HasBitWidth = D->isBitField() && D->getBitWidth();
  Expr *Init = D->getInClassInitializer();

  OF.emitTag("is_mutable");
  OF.emitBoolean(IsMutable);

  OF.emitTag("is_module_private");
  OF.emitBoolean(IsModulePrivate);

  OF.emitTag("bit_width_expr");
  if (HasBitWidth) {
    ObjectScope oScope(OF, 1);
    dumpStmt(D->getBitWidth());
  } else {
    OF.emitString("None");
  }

  OF.emitTag("init_expr");
  if (Init) {
    dumpStmt(Init);
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::VarDeclTupleSize() {
  return ASTExporter::DeclaratorDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitVarDecl(const VarDecl *D) {

  OF.emitTag("value_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitValueDecl(D);
  }

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
    def_expr = const_cast<Expr *>(ParmDecl->getDefaultArg());
  }
  bool HasDefault = (bool)def_expr;
  bool HasParmIndex = (bool)ParmDecl;
  bool isInitExprCXX11ConstantExpr = false;

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
    OF.emitString("None");
  }

  OF.emitTag("is_init_expr_cxx11_constant");
  OF.emitBoolean(isInitExprCXX11ConstantExpr);

  OF.emitTag("parm_index_in_function");
  if (HasParmIndex) {
    OF.emitInteger(ParmDecl->getFunctionScopeIndex());
  } else {
    OF.emitString("None");
  }

  OF.emitTag("default_value");
  if (HasDefault) {
    dumpDefaultArgStr(D->getInit());
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

  OF.emitTag("decl");
  {
    ObjectScope oScope(OF, 1);
    VisitDecl(D);
  }

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

  OF.emitTag("named_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitNamedDecl(D);
  }

  bool HasNominatedNamespace = D->getNominatedNamespace();

  OF.emitTag("using_location");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceLocation(D->getUsingLoc());
  }

  OF.emitTag("namespace_key_location");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceLocation(D->getNamespaceKeyLocation());
  }

  OF.emitTag("nested_name_specifier_locs");
  { dumpNestedNameSpecifierLoc(D->getQualifierLoc()); }

  OF.emitTag("nominated_namespace");
  if (HasNominatedNamespace) {
    ObjectScope oScope(OF, 1);
    NamespaceDecl const *ND = D->getNominatedNamespace();
    if (NamePrint.goodDeclName(cast<NamedDecl>(ND))) {
        dumpDeclRef(
                *ND,
                inMainFile(ND));
    } else {
        dumpName(*ND);
    }
  } else {
    OF.emitString("None");
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

  OF.emitTag("named_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitNamedDecl(D);
  }

  OF.emitTag("namespace_loc");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceLocation(D->getNamespaceLoc());
  }

  OF.emitTag("target_name_loc");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceLocation(D->getTargetNameLoc());
  }

  OF.emitTag("nested_name_specifier_locs");
  dumpNestedNameSpecifierLoc(D->getQualifierLoc());

  OF.emitTag("namespace");
  {
    ObjectScope oScope(OF, 1);
    NamespaceDecl const *ND = D->getNamespace();
    if (NamePrint.goodDeclName(cast<NamedDecl>(ND))) {
        dumpDeclRef(
                *ND,
                inMainFile(ND));
    } else {
        dumpName(*ND);
    }
  }

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
      OF.emitString("None");
    }
  }

  OF.emitTag("captured_var");
  if (decl) {
    ObjectScope oScope(OF, 1);
    if (NamePrint.goodDeclName(cast<NamedDecl>(decl))) {
        dumpDeclRef(*decl);
    } else {
        OF.emitString("None");
    }
  } else {
    OF.emitString("None");
  }

  OF.emitTag("is_implicit");
  OF.emitBoolean(IsImplicit);

  OF.emitTag("location");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceRange(source_range);
  }

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

  OF.emitTag("record");
  {
    ObjectScope oScope(OF, 1);
    VisitRecordDecl(D);
  }

  bool isComplete = D->isCompleteDefinition();

  SmallVector<CXXBaseSpecifier, 2> nonVBases;
  SmallVector<CXXBaseSpecifier, 2> vBases;
  if (isComplete) {
    for (const auto base : D->bases()) {
      if (base.isVirtual()) {
        vBases.push_back(base);
      } else {
        nonVBases.push_back(base);
      }
    }
  }

  bool HasVBases = vBases.size() > 0;
  bool HasNonVBases = nonVBases.size() > 0;

  unsigned numTransitiveVBases = 0;
  if (isComplete) {
    numTransitiveVBases = D->getNumVBases();
  }
  bool HasTransitiveVBases = numTransitiveVBases > 0;

  bool IsPOD = false;
  if (isComplete) {
    D->isPOD();
  }

  const CXXDestructorDecl *DestructorDecl = NULL;
  if (isComplete) {
    DestructorDecl = D->getDestructor();
  }

  const CXXMethodDecl *LambdaCallOperator = NULL;
  if (isComplete) {
    LambdaCallOperator = D->getLambdaCallOperator();
  }

  decltype(D->captures_begin()) I;
  decltype(D->captures_end()) E;

  if (isComplete) {
    I = D->captures_begin();
    E = D->captures_end();
  }

  OF.emitTag("is_complete");
  OF.emitBoolean(isComplete);

  OF.emitTag("is_polymorphic");
  OF.emitBoolean(isComplete && D->isPolymorphic());

  OF.emitTag("is_abstract");
  OF.emitBoolean(isComplete && D->isAbstract());

  OF.emitTag("bases");
  if (isComplete && (HasNonVBases || HasVBases || HasTransitiveVBases)) {

    ArrayScope aScope(OF, D->bases().end() - D->bases().begin());
    for (const auto base : D->bases()) {
      ObjectScope Scope(OF, 4);
      OF.emitTag("type");
      Type const *baseType = base.getType().getTypePtr();
      dumpQualTypeNoQuals(base.getType());

      if (dyn_cast<TagType>(baseType)) {
          TagDecl const *tagDecl = cast<TagType>(baseType)->getDecl();
          decls_inherited.push_back(tagDecl);
      }
      if (dyn_cast<TypedefType>(baseType)) {
          TypedefNameDecl const *tDecl = cast<TypedefType>(baseType)->getDecl();
          decls_inherited.push_back(tDecl);
      }

      dumpAccessSpecifier(base.getAccessSpecifier());

      OF.emitTag("is_virtual");
      OF.emitBoolean(base.isVirtual());

      OF.emitTag("is_transitive");
      OF.emitBoolean(false);
    }

    for (const auto base : D->vbases()) {

      OF.emitTag("type");
      dumpQualTypeNoQuals(base.getType());

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
  OF.emitBoolean(isComplete && IsPOD);

  OF.emitTag("destructor");
  if (isComplete && DestructorDecl) {
    ObjectScope oScope(OF, 1);
    if (NamePrint.goodDeclName(cast<NamedDecl>(DestructorDecl))) {
        dumpDeclRef(*DestructorDecl);
    } else {
        OF.emitString("None");
    }
  } else {
    OF.emitString("None");
  }

  OF.emitTag("lambda_call_operator");
  if (isComplete && LambdaCallOperator) {
    ObjectScope oScope(OF, 1);
    if (NamePrint.goodDeclName(cast<NamedDecl>(LambdaCallOperator))) {
        dumpDeclRef(*LambdaCallOperator);
    } else {
        OF.emitString("None");
    }
  } else {
    OF.emitString("None");
  }

  OF.emitTag("lambda_captures");
  if (isComplete && I != E) {
    ArrayScope Scope(OF, std::distance(I, E));
    for (; I != E; ++I) {
      ObjectScope oScope(OF, 1);
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
    { OF.emitString("None"); }

    OF.emitTag("pointer");
    { OF.emitString("None"); }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    { ArrayScope aScope(OF, 0); }

    break;
  case TemplateArgument::Type: {
    OF.emitString("Type");

    OF.emitTag("type");
    {
      ObjectScope oScope(OF, 1);
      dumpQualType(Arg.getAsType());
    }

    OF.emitTag("pointer");
    { OF.emitString("None"); }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    { ArrayScope aScope(OF, 0); }

    break;
  }
  case TemplateArgument::Declaration: {
    OF.emitString("Declaration");

    OF.emitTag("type");
    { OF.emitString("None"); }

    OF.emitTag("pointer");
    dumpDeclPointer(Arg.getAsDecl());

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    { ArrayScope aScope(OF, 0); }

    break;
  }
  case TemplateArgument::NullPtr:

    OF.emitString("NullPtr");

    OF.emitTag("type");
    { OF.emitString("None"); }

    OF.emitTag("pointer");
    { OF.emitString("None"); }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    { ArrayScope aScope(OF, 0); }

    break;
  case TemplateArgument::Integral: {
    OF.emitString("Integral");

    OF.emitTag("type");
    { OF.emitString("None"); }

    OF.emitTag("pointer");
    { OF.emitString("None"); }

    OF.emitTag("integer");
    OF.emitString(Arg.getAsIntegral().toString(10));

    OF.emitTag("parameter_pack");
    { ArrayScope aScope(OF, 0); }

    break;
  }
  case TemplateArgument::Template: {

    OF.emitString("Template");

    OF.emitTag("type");
    { OF.emitString("None"); }

    OF.emitTag("pointer");
    { OF.emitString("None"); }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    { ArrayScope aScope(OF, 0); }

    break;
  }
  case TemplateArgument::TemplateExpansion: {
    OF.emitString("TemplateExpansion");

    OF.emitTag("type");
    { OF.emitString("None"); }

    OF.emitTag("pointer");
    { OF.emitString("None"); }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    { ArrayScope aScope(OF, 0); }

    break;
  }
  case TemplateArgument::Expression: {
    OF.emitString("Expression");

    OF.emitTag("type");
    { OF.emitString("None"); }

    OF.emitTag("pointer");
    { OF.emitString("None"); }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    { ArrayScope aScope(OF, 0); }

    break;
  }
  case TemplateArgument::Pack: {
    OF.emitString("Pack");

    OF.emitTag("type");
    { OF.emitString("None"); }

    OF.emitTag("pointer");
    { OF.emitString("None"); }

    OF.emitTag("integer");
    OF.emitString("None");

    OF.emitTag("parameter_pack");
    ArrayScope aScope(OF, Arg.pack_size());
    for (TemplateArgument::pack_iterator I = Arg.pack_begin(),
                                         E = Arg.pack_end();
         I != E;
         ++I) {
      ObjectScope oScope(OF, 1);
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
  OF.emitTag("template_decl");
  dumpDeclPointer(D);

  OF.emitTag("specialization_args");
  if (HasTemplateArgs) {
    ArrayScope aScope(OF, Args.size());
    for (size_t i = 0; i < Args.size(); i++) {
      ObjectScope oScope(OF, 1);
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

  OF.emitTag("cxx_record");
  {
    ObjectScope oScope(OF, 1);
    VisitCXXRecordDecl(D);
  }

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

  OF.emitTag("specialization");
  {
    ObjectScope oScope(OF, 1);
    dumpTemplateSpecialization(D->getSpecializedTemplate(),
                               D->getTemplateArgs());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CXXMethodDeclTupleSize() {
  return FunctionDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCXXMethodDecl(const CXXMethodDecl *D) {
  OF.emitTag("function");
  {
    ObjectScope oScope(OF, 1);
    VisitFunctionDecl(D);
  }
  bool IsVirtual = D->isVirtual();
  bool IsStatic = D->isStatic();
  const CXXConstructorDecl *C = dyn_cast<CXXConstructorDecl>(D);
  bool HasCtorInitializers = C && C->init_begin() != C->init_end();
  bool IsConstexpr = D->isConstexpr();
  auto OB = D->begin_overridden_methods();
  auto OE = D->end_overridden_methods();

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
      ObjectScope oScope(OF, 1);
      dumpCXXCtorInitializer(*I);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("overriden_methods");
  if (OB != OE) {
    ArrayScope Scope(OF, std::distance(OB, OE));
    for (; OB != OE; ++OB) {
      ObjectScope oScope(OF, 1);
      bool isNamed = isa<NamedDecl>(*OB);
      if (isNamed && !NamePrint.goodDeclName(cast<NamedDecl>(*OB))) {
          continue;
      }
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
void ASTExporter<ATDWriter>::VisitCXXConstructorDecl(
    const CXXConstructorDecl *D) {

  OF.emitTag("ctor");
  {
    ObjectScope oScope(OF, 1);
    VisitCXXMethodDecl(D);
  }

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

  OF.emitTag("named_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitNamedDecl(D);
  }

  OF.emitTag("cxx_record");
  {
    ObjectScope oScope(OF, 1);
    VisitCXXRecordDecl(D->getTemplatedDecl());
  }

  std::vector<const ClassTemplateSpecializationDecl *> DeclsToDump;
  llvm::SmallVector<ClassTemplatePartialSpecializationDecl *, 4> partials;
  ClassTemplateDecl *DNC = const_cast<ClassTemplateDecl *>(D);
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

  OF.emitTag("parameters");
  if (hasParams) {
    ArrayScope aScope(OF, pList->end() - pList->begin());
    for (auto &p : *pList) {
      TemplateTypeParmDecl *ttype = dyn_cast<TemplateTypeParmDecl>(p);
      NonTypeTemplateParmDecl *nt = dyn_cast<NonTypeTemplateParmDecl>(p);
      TemplateTemplateParmDecl *ttemp = dyn_cast<TemplateTemplateParmDecl>(p);

      ObjectScope oScope(OF, 1);
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
      ObjectScope oScope(OF, 1);
      VisitClassTemplateSpecializationDecl(spec);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("partial_specializations");
  if (hasPartials) {
    ArrayScope aScope(OF, partials.end() - partials.begin());
    for (auto ptl : partials) {
      ObjectScope oScope(OF, 1);
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

  OF.emitTag("named_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitNamedDecl(D);
  }

  OF.emitTag("function");
  {
    ObjectScope oScope(OF, 1);
    VisitFunctionDecl(D->getTemplatedDecl());
  }
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

  OF.emitTag("parameters");
  if (hasParams) {
    ArrayScope aScope(OF, pList->end() - pList->begin());
    for (auto &p : *pList) {
      TemplateTypeParmDecl *ttype = dyn_cast<TemplateTypeParmDecl>(p);
      NonTypeTemplateParmDecl *nt = dyn_cast<NonTypeTemplateParmDecl>(p);
      TemplateTemplateParmDecl *ttemp = dyn_cast<TemplateTemplateParmDecl>(p);
      ObjectScope oScope(OF, 1);
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

  OF.emitTag("decl");
  {
    ObjectScope oScope(OF, 1);
    VisitDecl(D);
  }

  OF.emitTag("kind");
  if (TypeSourceInfo *T = D->getFriendType()) {
    OF.emitString("Type");

    OF.emitTag("type");
    { dumpQualTypeNoQuals(T->getType()); }

    OF.emitTag("friend");
    { OF.emitString("None"); }

  } else {
    OF.emitString("Decl");

    OF.emitTag("type");
    { OF.emitString("None"); }

    OF.emitTag("friend");
    { dumpDecl(D->getFriendDecl()); }
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypeAliasDeclTupleSize() {
  return ASTExporter::TypedefNameDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTypeAliasDecl(const TypeAliasDecl *D) {

  OF.emitTag("type_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitTypeDecl(D);
  }

  TypeAliasTemplateDecl *dtemplate = D->getDescribedAliasTemplate();
  bool describes_template = dtemplate != nullptr;

  OF.emitTag("underlying_type");
  {
    QualType const &qt = D->getUnderlyingType();
    if (typeIsHidden(qt)) {
        OF.emitString("None");
    } else {
        ObjectScope oScope(OF, 1);
        dumpQualType(qt);
    }

  }

  OF.emitTag("described_template");
  if (describes_template) {
    dumpDeclPointer(dtemplate);
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypeAliasTemplateDeclTupleSize() {
  return TypeAliasDeclTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTypeAliasTemplateDecl(
    const TypeAliasTemplateDecl *D) {

  OF.emitTag("type_alias_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitTypeAliasDecl(D->getTemplatedDecl());
  }

  TemplateParameterList *pList = D->getTemplateParameters();
  bool hasParams = !(pList->begin() == pList->end());

  TypeAliasTemplateDecl *member_template =
      D->getInstantiatedFromMemberTemplate();
  bool mTemp = member_template != nullptr;

  OF.emitTag("canonical_decl");
  dumpDeclPointer(D->getCanonicalDecl());

  OF.emitTag("parameters");
  if (hasParams) {
    ArrayScope aScope(OF, pList->end() - pList->begin());
    for (auto &p : *pList) {
      TemplateTypeParmDecl *ttype = dyn_cast<TemplateTypeParmDecl>(p);
      NonTypeTemplateParmDecl *nt = dyn_cast<NonTypeTemplateParmDecl>(p);
      TemplateTemplateParmDecl *ttemp = dyn_cast<TemplateTemplateParmDecl>(p);

      ObjectScope oScope(OF, 1);
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
    dumpDeclPointer(member_template);
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitClassTemplatePartialSpecializationDecl(
    const ClassTemplatePartialSpecializationDecl *D) {

  OF.emitTag("class_template_specialization");
  {
    ObjectScope oScope(OF, 1);
    VisitClassTemplateSpecializationDecl(D);
  }
  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpTemplateTypeParmDecl(
    const TemplateDecl *T, const TemplateTypeParmDecl *D) {

  bool const has_default = D->hasDefaultArgument();
  bool const used_typename = D->wasDeclaredWithTypename();
  bool const is_pack = D->isParameterPack();

  OF.emitTag("type_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitTypeDecl(D);
  }

  OF.emitTag("template_decl");
  dumpDeclPointer(T);

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
    ObjectScope oScope(OF, 1);
    dumpQualType(D->getDefaultArgument());
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpNonTypeTemplateParmDecl(
    const TemplateDecl *T, const NonTypeTemplateParmDecl *D) {

  bool const has_default = D->hasDefaultArgument();
  bool const is_pack = D->isParameterPack();

  OF.emitTag("value_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitValueDecl(D);
  }

  OF.emitTag("param_type");
  OF.emitString("TemplateNonTypeParam");

  OF.emitTag("template_decl");
  dumpDeclPointer(T);

  unsigned int p_idx = D->getIndex();
  OF.emitTag("index");
  OF.emitInteger(p_idx);

  p_idx = D->getDepth();
  OF.emitTag("depth");
  OF.emitInteger(p_idx);

  OF.emitTag("is_parameter_pack");
  OF.emitBoolean(is_pack);
  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(D->getType());
  }

  OF.emitTag("default");
  if (has_default && D->getDefaultArgument()) {
    dumpDefaultArgStr(D->getDefaultArgument());
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpDefaultArgStr(Expr const *E) {
    
    SourceLocation start_tok = E->getBeginLoc();
    SourceLocation end_tok = E->getEndLoc();

    unsigned int end_tok_len = Lexer::MeasureTokenLength(
            end_tok, Context.getSourceManager(), Context.getLangOpts()
            );

    SourceLocation end_tok_end = end_tok.getLocWithOffset(end_tok_len);
    SourceRange def_arg_range(start_tok, end_tok_end);

    OF.emitString(
            Lexer::getSourceText(
                CharSourceRange::getCharRange(
                    def_arg_range
                    ),
                Context.getSourceManager(),
                Context.getLangOpts()
                ).str()
            );

    return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpTemplateTemplateParmDecl(
    const TemplateDecl *T, const TemplateTemplateParmDecl *D) {

  bool const has_default = D->hasDefaultArgument();
  bool const is_pack = D->isParameterPack();
  unsigned int idx = D->getIndex();

  OF.emitTag("named_decl");
  {
    ObjectScope oScope(OF, 1);
    VisitNamedDecl(D);
  }

  OF.emitTag("param_type");
  OF.emitString("TemplateTemplateParam");

  OF.emitTag("template_decl");
  dumpDeclPointer(T);

  OF.emitTag("index");
  OF.emitInteger(idx);

  idx = D->getDepth();
  OF.emitTag("depth");
  OF.emitInteger(idx);

  OF.emitTag("is_parameter_pack");
  OF.emitBoolean(is_pack);

  OF.emitTag("default");
  if (has_default) {
    TemplateName def_temp =
        D->getDefaultArgument().getArgument().getAsTemplateOrTemplatePattern();
    TemplateDecl *decl = def_temp.getAsTemplateDecl();
    dumpDeclPointer(decl);
  } else {
    OF.emitString("None");
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

  ObjectScope oScope(OF, 3);
  if (!S) {
    // We use a fixed NullStmt node to represent null pointers
    S = NullPtrStmt;
  }

  OF.emitTag("clang_kind");
  OF.emitString("Stmt");

  OF.emitTag("kind");
  OF.emitString(std::string(S->getStmtClassName()));

  OF.emitTag("content");
  {
    ObjectScope oScope(OF, 1);
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

  OF.emitTag("pointer");
  dumpPointer(S);
  OF.emitTag("location");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceRange(S->getSourceRange());
  }

  OF.emitTag("content");
  {
    TupleScope Scope(OF, std::distance(S->child_begin(), S->child_end()));
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
  OF.emitTag("stmt");

  {
    ObjectScope oScope(OF, 3);
    VisitStmt(Node);
  }

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
  if (typeIsHidden(Node->getType())) {
      OF.emitTag("skipped");
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("Node type is hidden");
      OF.emitTag("pointer");
      dumpPointer(Node);
      return;
  }
  VisitStmt(Node);

  ExprValueKind VK = Node->getValueKind();
  bool HasNonDefaultValueKind = VK != VK_RValue;
  ExprObjectKind OK = Node->getObjectKind();
  bool HasNonDefaultObjectKind = OK != OK_Ordinary;

  OF.emitTag("expr");
  ObjectScope Scope(OF, 3);

  OF.emitTag("qual_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(Node->getType());
  }

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

  OF.emitTag("name");
  OF.emitString(RD->getName());

  OF.emitTag("template");
  if (describesTemplate) {
    ObjectScope oScope(OF, 1);
    dumpDeclPointer(T);
  } else {
    OF.emitString("None");
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
  OF.emitTag("expr");

  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Node);
  }

  const ValueDecl *D = Node->getDecl();
  const NamedDecl *FD = Node->getFoundDecl();
  bool HasFoundDeclRef = FD && D != FD;
  bool isNamed = false;

  OF.emitTag("decl_ref");
  if (D) {
    isNamed = isa<NamedDecl>(D);
    if (isNamed && NamePrint.goodDeclName(cast<NamedDecl>(D))) {
        ObjectScope oScope(OF, 1);
        dumpDeclRef(*D);
    } else {
        OF.emitString("None");
    }
  } else {
    OF.emitString("None");
  }

  OF.emitTag("found_decl_ref");
  if (HasFoundDeclRef) {
    ObjectScope oScope(OF, 1);
    isNamed = isa<NamedDecl>(FD);
    if (isNamed && NamePrint.goodDeclName(cast<NamedDecl>(FD))) {
        dumpDeclRef(*FD);
    } else {
        OF.emitString("None");
    }
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::OverloadExprTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitOverloadExpr(const OverloadExpr *Node) {

  OF.emitTag("expr");
  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Node);
  }

  bool HasDecls = Node->getNumDecls() > 0;

  OF.emitTag("decls");
  if (HasDecls) {
    ArrayScope Scope( // not covered by tests
        OF,
        std::distance(Node->decls_begin(), Node->decls_end()));
    for (auto I : Node->decls()) {
      ObjectScope oScope(OF, 1);
      bool isNamed = isa<NamedDecl>(I);
      if (isNamed && !NamePrint.goodDeclName(I)) {
          continue;
      }
      dumpDeclRef(*I);
    }
  } else {
    ArrayScope aScope(OF, 0);
  }

  OF.emitTag("name");
  {
    ObjectScope oScope(OF, 1);
    dumpDeclarationName(Node->getName());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CharacterLiteralTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitCharacterLiteral(
    const CharacterLiteral *Node) {

  OF.emitTag("expr");
  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Node);
  }

  OF.emitTag("value");
  OF.emitInteger(Node->getValue());

  return;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::emitAPInt(bool isSigned,
                                       const llvm::APInt &value) {

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

  OF.emitTag("expr");
  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Node);
  }

  const auto value = Node->getValue();

  OF.emitTag("value");
  {
    ObjectScope oScope(OF, 1);
    this->emitAPInt(Node->getType()->isSignedIntegerType(), value);
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FixedPointLiteralTupleSize() {
  return ExprTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFixedPointLiteral(
    const FixedPointLiteral *Node) {

  OF.emitTag("expr");
  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Node);
  }
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

  OF.emitTag("expr");
  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Node);
  }

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

  OF.emitTag("expr");
  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Str);
  }

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

  OF.emitTag("expr");
  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Node);
  }

  bool IsArrow = Node->isArrow();
  LangOptions LO;
  // ignore real lang options - it will get it wrong when compiling
  // with -fapple-kext flag
  bool PerformsVirtualDispatch = Node->performsVirtualDispatch(LO);

  OF.emitTag("is_arrow");
  OF.emitBoolean(IsArrow);

  OF.emitTag("performs_virtual_dispatch");
  OF.emitBoolean(PerformsVirtualDispatch);

  OF.emitTag("id");
  ValueDecl *memberDecl = Node->getMemberDecl();
  {
    ObjectScope oScope(OF, 1);
    dumpName(*memberDecl);
  }

  OF.emitTag("decl_ref");
  {
    ObjectScope oScope(OF, 1);
    bool isNamed = isa<NamedDecl>(memberDecl);
    if (isNamed && NamePrint.goodDeclName(cast<NamedDecl>(memberDecl))) {
        dumpDeclRef(*memberDecl);
    } else {
        OF.emitString("None");
    }
  }

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

  OF.emitTag("expr");
  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Node);
  }

  const Expr *InitExpr = Node->getExpr();

  OF.emitTag("init_expr");
  if (InitExpr) {
    dumpStmt(InitExpr);
  } else {
    OF.emitString("None");
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

  OF.emitTag("expr");
  {
    ObjectScope oScope(OF, 1);
    VisitExpr(Node);
  }

  const Expr *InitExpr = Node->getExpr();

  OF.emitTag("init_expr");
  if (InitExpr) {
    dumpStmt(InitExpr);
  } else {
    OF.emitString("None");
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
  building_comment = false;

  OF.emitTag("parent_pointer");
  dumpPointer(C);

  OF.emitTag("location");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceRange(C->getSourceRange());
  }

  dumpComment(C);

  OF.emitTag("text");
  OF.emitString(comment_text);

  FC = 0;
}

//#define COMMENT(CLASS, PARENT) //@atd #define @CLASS@_tuple @PARENT@_tuple
//#define ABSTRACT_COMMENT(COMMENT) COMMENT
//#include <clang/AST/CommentNodes.inc>
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpComment(const Comment *C) {

  auto process_string =
      [&bc = building_comment](std::string to_process) -> std::string {
    std::string out = to_process;

    out.erase(out.begin(), std::find_if(out.begin(), out.end(), [](char c) {
                return !std::isspace(static_cast<unsigned char>(c));
              }));
    out.erase(
        std::find_if(
            out.rbegin(),
            out.rend(),
            [](char c) { return !std::isspace(static_cast<unsigned char>(c)); })
            .base(),
        out.end());
    out = bc ? std::string(" ") + out : out;
    bc = true;
    return out;
  };

  if (dyn_cast<TextComment>(C)) {
    comment_text += process_string(dyn_cast<TextComment>(C)->getText().str());
  } else if (dyn_cast<VerbatimBlockLineComment>(C)) {
    comment_text +=
        process_string(dyn_cast<VerbatimBlockLineComment>(C)->getText().str());
  } else if (dyn_cast<VerbatimLineComment>(C)) {
    comment_text +=
        process_string(dyn_cast<VerbatimLineComment>(C)->getText().str());
  }

  if (!C) {
    // We use a fixed NoComment node to represent null pointers
    C = NullPtrComment;
  }

  { ConstCommentVisitor<ASTExporter<ATDWriter>>::visit(C); }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::CommentTupleSize() {
  return 0;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::visitComment(const Comment *C) {
  {
    Comment::child_iterator I = C->child_begin(), E = C->child_end();
    for (; I != E; ++I) {
      dumpComment(*I);
    }
  }
  return;
}

// PICKUP HERE
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpType(const Type *T) {

  std::string typeClassName = T ? T->getTypeClassName() : "None";

  OF.emitTag("clang_kind");
  OF.emitString("Type");

  OF.emitTag("kind");
  OF.emitString(typeClassName + "Type");

  OF.emitTag("content");
  {
    if (T) {
      // TypeVisitor assumes T is non-null
      ObjectScope oScope(OF, 1);
      TypeVisitor<ASTExporter<ATDWriter>>::Visit(T);
    } else {
      ObjectScope oScope(OF, 1);
      VisitType(nullptr);
    }
  }
  return;

}

template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpPointerToType(const Type *T) {
  bool alreadyReferenced = false;
  for (auto &type : types_referenced) {
      if (type == T) {
          alreadyReferenced = true;
          break;
      }
  }
  if (!alreadyReferenced) {
      types_referenced.push_back(T);
      new_types_referenced.push_back(T);
  }
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

  OF.emitTag("pointer");
  dumpPointer(T);

  OF.emitTag("desugared_type");
  if (HasDesugaredType) {
    dumpPointerToType(T->getUnqualifiedDesugaredType());
  } else {
    OF.emitString("None");
  }

  if (!OF.block()) {
    types_written.push_back(T);
  }
  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::AdjustedTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitAdjustedType(const AdjustedType *T) {

  if (typeIsHidden(T->getAdjustedType())) {
      OF.emitTag("skipped");
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("Adjusted type is hidden");
      OF.emitTag("pointer");
      dumpPointer(T);
      return;
  }

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("qual_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getAdjustedType());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ArrayTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitArrayType(const ArrayType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  QualType EltT = T->getElementType();
  bool HasStride = hasMeaningfulTypeInfo(EltT.getTypePtr());

  OF.emitTag("element_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(EltT);
  }

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

  OF.emitTag("array_type");
  {
    ObjectScope oScope(OF, 1);
    VisitArrayType(T);
  }

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

  OF.emitTag("array_type");
  {
    ObjectScope oScope(OF, 1);
    VisitArrayType(T);
  }

  OF.emitTag("size_expr");
  dumpPointer(T->getSizeExpr());

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::AtomicTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitAtomicType(const AtomicType *T) {

  if (typeIsHidden(T->getValueType())) {
      OF.emitTag("skipped");
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("Value type is hidden");
      OF.emitTag("pointer");
      dumpPointer(T);
      return;
  }

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("qual_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getValueType());
  }

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

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

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

  if (typeIsHidden(T->getPointeeType())) {
      OF.emitTag("skipped");
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("Pointee type is hidden");
      OF.emitTag("pointer");
      dumpPointer(T);
      return;
  }

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("qual_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getPointeeType());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::BuiltinTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitBuiltinType(const BuiltinType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

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

  if (typeIsHidden(T->getUnderlyingType())) {
      OF.emitTag("skipped");
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("Underlying type is hidden");
      OF.emitTag("pointer");
      dumpPointer(T);
      return;
  }

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("qual_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getUnderlyingType());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FunctionTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFunctionType(const FunctionType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("return_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getReturnType());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::FunctionProtoTypeTupleSize() {
  return FunctionTypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitFunctionProtoType(
    const FunctionProtoType *T) {

  OF.emitTag("function_type");
  {
    ObjectScope oScope(OF, 1);
    VisitFunctionType(T);
  }

  bool HasParamsType = T->getNumParams() > 0;

  OF.emitTag("param_types");
  if (HasParamsType) {
    ArrayScope aScope(OF, T->getParamTypes().size());
    for (const auto &paramType : T->getParamTypes()) {
      ObjectScope oScope(OF, 1);
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

  if (typeIsHidden(T->getPointeeType())) {
      OF.emitTag("skipped");
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("Pointee type is hidden");
      OF.emitTag("pointer");
      dumpPointer(T);
      return;
  }

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("qual_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getPointeeType());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ParenTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitParenType(const ParenType *T) {
  // this is just syntactic sugar

  if (typeIsHidden(T->getInnerType())) {
      OF.emitTag("skipped");
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("Inner type is hidden");
      OF.emitTag("pointer");
      dumpPointer(T);
      return;
  }

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("qual_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getInnerType());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::PointerTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitPointerType(const PointerType *T) {

  if (typeIsHidden(T->getPointeeType())) {
      OF.emitTag("skipped");
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("Pointee type is hidden");
      OF.emitTag("pointer");
      dumpPointer(T);
      return;
  }

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("qual_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getPointeeType());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::ReferenceTypeTupleSize() {
  return TypeWithChildInfoTupleSize();
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitReferenceType(const ReferenceType *T) {

  if (typeIsHidden(T->getPointeeType())) {
      OF.emitTag("skipped");
      OF.emitBoolean(true);
      OF.emitTag("reason");
      OF.emitString("Pointee type is hidden");
      OF.emitTag("pointer");
      dumpPointer(T);
      return;
  }

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("qual_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getPointeeType());
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TagTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTagType(const TagType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("decl_pointer");
  dumpDeclPointer(T->getDecl());

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TypedefTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTypedefType(const TypedefType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  OF.emitTag("child_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->desugar());
  }
  OF.emitTag("decl_pointer");
  dumpDeclPointer(T->getDecl());

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TemplateTypeParmTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTemplateTypeParmType(
    const TemplateTypeParmType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  bool isSugared = T->isSugared();

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

  OF.emitTag("parameter");
  dumpDeclPointer(T->getDecl());

  OF.emitTag("desugared_type");
  if (isSugared) {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->desugar());
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::SubstTemplateTypeParmTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitSubstTemplateTypeParmType(
    const SubstTemplateTypeParmType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  bool isSugared = T->isSugared();

  OF.emitTag("replaced");
  dumpPointerToType(T->getReplacedParameter());

  OF.emitTag("replacement_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getReplacementType());
  }

  OF.emitTag("desugared_type");
  if (isSugared) {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->desugar());
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::TemplateSpecializationTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitTemplateSpecializationType(
    const TemplateSpecializationType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  bool isSugared = T->isSugared();
  bool isAlias = T->isTypeAlias();
  unsigned int nArgs = T->getNumArgs();
  bool hasArgs = nArgs > 0;

  OF.emitTag("type_alias");
  OF.emitBoolean(isAlias);

  OF.emitTag("template_decl");
  dumpDeclPointer(T->getTemplateName().getAsTemplateDecl());

  OF.emitTag("aliased_type");
  if (isAlias) {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getAliasedType());
  } else {
    OF.emitString("None");
  }

  OF.emitTag("desugared_type");
  if (isSugared) {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->desugar());
  } else {
    OF.emitString("None");
  }

  TemplateArgument const *args = T->getArgs();

  OF.emitTag("specialization_args");
  if (args && hasArgs) {
    ArrayScope aScope(OF, nArgs);
    for (unsigned int arg_idx = 0; arg_idx < nArgs; ++arg_idx) {
      ObjectScope oScope(OF, 1);
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
void ASTExporter<ATDWriter>::VisitInjectedClassNameType(
    const InjectedClassNameType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  bool isSugared = T->isSugared();

  OF.emitTag("injected_specialization_type");
  {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->getInjectedSpecializationType());
  }

  OF.emitTag("desugared_type");
  if (isSugared) {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->desugar());
  } else {
    OF.emitString("None");
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::DependentNameTypeTupleSize() {
  return TypeTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitDependentNameType(
    const DependentNameType *T) {

  OF.emitTag("type");
  {
    ObjectScope oScope(OF, 1);
    VisitType(T);
  }

  bool isSugared = T->isSugared();

  OF.emitTag("identifier");
  OF.emitString(T->getIdentifier()->getName().str());

  OF.emitTag("desugared_type");
  if (isSugared) {
    ObjectScope oScope(OF, 1);
    dumpQualType(T->desugar());
  } else {
    OF.emitString("None");
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

  OF.emitTag("clang_kind");
  OF.emitString("Attr");

  OF.emitTag("kind");
  OF.emitString(tag);

  OF.emitTag("content");
  {
    ObjectScope oScope(OF, 1);
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
  OF.emitTag("pointer");
  dumpPointer(A);
  OF.emitTag("location");
  {
    ObjectScope oScope(OF, 1);
    dumpSourceRange(A->getRange());
  }
  OF.emitTag("attr");
  OF.emitString(std::string(A->getSpelling()));

  return;
}
template <class ATDWriter>
void ASTExporter<ATDWriter>::dumpVersionTuple(const VersionTuple &VT) {
  Optional<unsigned> minor = VT.getMinor();
  Optional<unsigned> subminor = VT.getSubminor();
  Optional<unsigned> build = VT.getBuild();

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

  OF.emitTag("attr");
  {
    ObjectScope oScope(OF, 1);
    VisitAttr(A);
  }

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

  OF.emitTag("attr");
  {
    ObjectScope oScope(OF, 1);
    VisitAttr(A);
  }

  {
    IdentifierInfo *platform = A->getPlatform();

    OF.emitTag("platform");
    if (platform != nullptr) {
      OF.emitString(platform->getNameStart());
    } else {
      OF.emitString("None");
    }

    OF.emitTag("introduced");
    {
      ObjectScope oScope(OF, 1);
      dumpVersionTuple(A->getIntroduced());
    }
  }

  return;
}

template <class ATDWriter>
int ASTExporter<ATDWriter>::SentinelAttrTupleSize() {
  return AttrTupleSize() + 1;
}

template <class ATDWriter>
void ASTExporter<ATDWriter>::VisitSentinelAttr(const SentinelAttr *A) {

  OF.emitTag("attr");
  {
    ObjectScope oScope(OF, 1);
    VisitAttr(A);
  }

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

  OF.emitTag("attr");
  {
    ObjectScope oScope(OF, 1);
    VisitAttr(A);
  }

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
  std::shared_ptr<ASTPluginLib::IncludesPreprocessorHandlerData> preproc;
  std::unique_ptr<raw_ostream> OS;

 public:
  using ASTConsumerOptions = ASTLib::ASTExporterOptions;
  using PreprocessorHandler = ASTPluginLib::IncludesPreprocessorHandler;
  using PreprocessorHandlerData = ASTPluginLib::IncludesPreprocessorHandlerData;

  ExporterASTConsumer(const CompilerInstance &CI,
                      std::shared_ptr<ASTConsumerOptions> options,
                      std::shared_ptr<PreprocessorHandlerData> sharedData,
                      std::unique_ptr<raw_ostream> &&OS)
      : options(options), preproc(sharedData), OS(std::move(OS)) {}

  virtual void HandleTranslationUnit(ASTContext &Context) {
    TranslationUnitDecl *D = Context.getTranslationUnitDecl();
    ASTExporter<ATDWriter> P(*OS, Context, *options, *preproc);
    P.dumpDecl(D, true);
  }
};

typedef ASTPluginLib::SimplePluginASTAction<
    ASTLib::ExporterASTConsumer<ASTLib::JsonWriter>>
    JsonExporterASTAction;

} // end of namespace ASTLib

// Unused

#endif
