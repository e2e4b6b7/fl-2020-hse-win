module PrologAst where

type Var = String
type Identifier = String

data Token = TIdent Identifier
           | TVar Var
           | Comma
           | Semi
           | Lbr
           | Rbr
           | Dot
           | Cork
           deriving (Eq, Show)

data PrologProgram = Program {
        pModule :: Maybe Identifier
      , types   :: [TypeDef]
      , rels    :: [Relation]
      }
      deriving (Eq, Show)

data TypeDef = TypeDef Identifier Type
             deriving (Eq, Show)

data Type = Var Var
          | TAtom Atom
          | Arrow Type Type
          deriving (Eq, Show)

data Atom = Atom { atomHead :: Identifier, atomArgs :: [Either Atom Var] }
          deriving (Eq, Show)

data Relation = Relation { relHead :: Atom, relBody :: Maybe RelationBody }
              deriving (Eq, Show)

data RelationBody = RAtom Atom
                  | Conj RelationBody RelationBody
                  | Disj RelationBody RelationBody
                  deriving (Eq, Show)