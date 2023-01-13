--   root    - root file
-- ✓ cmd     - command
-- ✓ sub     - subcommand
-- ✓ flag    - flag
-- ✓ flagarg - flag argument
-- ✓ include - inclusion?
-- ✓ defargs - arguments definition
-- ✓ defopts - options definition
-- ✓ opts    - option specifier
-- ✓ opt     - optional prefix
-- ✓ reqd    - required prefix
-- ✓ arg     - argument
-- ✓ desc    - description
-- ✓ title   - title
-- ✓ ref     - reference
-- ✓ varargs - variable args
module AST where

import Data.List

data RootExpression
  = RootExpressionCmd Command
  | RootExpressionSub Subcommand
  | RootExpressionArgumentDefinition ArgumentsDefinition
  | RootExpressionOptionsDefinition OptionsDefinition
  | RootExpressionFlag Flag
  | RootExpressionOptionSpecifier OptionSpecifier

-- A reference to a value, starts with @
newtype Ref = Ref String
  deriving (Show)

newtype Include = Include Ref

data Argument = Argument
  { argumentOptional :: Bool,
    argumentName :: Maybe [String],
    argumentDescription :: Maybe String,
    argumentOptionSpecifier :: Maybe OptionSpecifier
  }
  deriving (Show)

data VariableArgument = VariableArgument
  { variableArgumentOptional :: Bool,
    variableArgumentValue :: Either Ref [String]
  }
  deriving (Show)

data FlagArgument = FlagArgument
  { flagArgumentNames :: Flag,
    flagArgumentDescription :: Maybe String,
    flagArgumentOptionSpecifier :: Maybe (Either Ref OptionSpecifier),
    flagArgumentRequired :: Bool
  }
  deriving (Show)

data SubcommandValue
  = SubcommandValueFlag Flag
  | SubcommandValueFlagArgument FlagArgument
  | SubcommandValueInclude Include
  | SubcommandValueArgument Argument
  | SubcommandValueSubcommand Subcommand

data Subcommand = Subcommand
  { subcommandName :: String,
    subCommandValues :: [SubcommandValue]
  }

data OptionsDefinition = OptionsDefinition
  { optionsDefinitionName :: String,
    optionsDefinitionSpecifier :: OptionSpecifier
  }

data ArgumentsDefinitionValue
  = ArgumentsDefinitionValueFlagArgument FlagArgument
  | ArgumentsDefinitionArgument Argument
  | ArgumentsDefinitionValueVariableArgument VariableArgument

data ArgumentsDefinition = ArgumentsDefinition
  { argumentsDefinitionName :: String,
    argumentsDefinitionValues :: [ArgumentsDefinitionValue]
  }

newtype Title = Title String

newtype Description = Description String

newtype Flag = Flag [String]
  deriving (Show)

data FlagDefinition = FlagDefinition
  { flagDefinitionFlags :: [Flag],
    flagDefinitionDescription :: Maybe String
  }
  deriving (Show)

data OptionSpecifier = OptionSpecifierShell String | OptionSpecifierConst [String]
  deriving (Show)

newtype Command = Command String
