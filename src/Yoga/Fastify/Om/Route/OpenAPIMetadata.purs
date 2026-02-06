module Yoga.Fastify.Om.Route.OpenAPIMetadata
  ( class HasDescription
  , description
  , class HasExample
  , example
  , class HasFormat
  , format
  , class HasMinimum
  , minimum
  , class HasMaximum
  , maximum
  , class HasPattern
  , pattern
  , class HasMinLength
  , minLength
  , class HasMaxLength
  , maxLength
  , class HasTitle
  , title
  , class HasNullable
  , nullable
  , class HasDefault
  , default
  , class HasDeprecated
  , deprecated
  , class HasEnum
  , enum
  , class GenericEnumValues
  , genericEnumValues
  , class HasOperationMetadata
  , operationMetadata
  , OperationMetadata
  , Description
  , Example
  , Format
  , Minimum
  , Maximum
  , Pattern
  , MinLength
  , MaxLength
  , Title
  , Nullable
  , Default
  , Deprecated
  , Enum
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as GR
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable, reflectType)
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign (Foreign)
import Prim.TypeError (class Warn, Text)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fastify.Om.Path (class ParseParam, parseParam)
import Yoga.Fastify.Om.Route.HeaderValue (class HeaderValue, parseHeader, printHeader, class HeaderValueType, headerValueType)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

--------------------------------------------------------------------------------
-- Metadata Type Wrappers
--------------------------------------------------------------------------------

-- | Attach a description to a type.
-- | Example: Int # Description "The unique identifier for a user"
data Description :: Symbol -> Type -> Type
data Description desc a

-- | Attach an example value to a type.
-- | Example: Int # Example "123"
data Example :: Symbol -> Type -> Type
data Example exampleValue a

-- | Attach a format annotation to a type.
-- | Example: String # Format "email"
data Format :: Symbol -> Type -> Type
data Format formatStr a

-- | Set a minimum value constraint.
-- | Example: Int # Minimum 1
data Minimum :: Int -> Type -> Type
data Minimum minVal a

-- | Set a maximum value constraint.
-- | Example: Int # Maximum 100
data Maximum :: Int -> Type -> Type
data Maximum maxVal a

-- | Set a regex pattern constraint.
-- | Example: String # Pattern "^[a-z]+$"
data Pattern :: Symbol -> Type -> Type
data Pattern pat a

-- | Set a minimum string length constraint.
-- | Example: String # MinLength 1
data MinLength :: Int -> Type -> Type
data MinLength minLen a

-- | Set a maximum string length constraint.
-- | Example: String # MaxLength 255
data MaxLength :: Int -> Type -> Type
data MaxLength maxLen a

-- | Attach a title to a type.
-- | Example: String # Title "UserName"
data Title :: Symbol -> Type -> Type
data Title t a

-- | Mark a type as nullable.
-- | Example: String # Nullable
data Nullable :: Type -> Type
data Nullable a

-- | Set a default value.
-- | Example: Int # Default "10"
data Default :: Symbol -> Type -> Type
data Default val a

-- | Mark a type as deprecated.
-- | Example: Int # Deprecated
data Deprecated :: Type -> Type
data Deprecated a

-- | Wrapper to use an enum type (sum type with no-argument constructors) in routes.
-- | The type parameter should be a Generic sum type, and enum values will be
-- | automatically extracted from its constructor names.
-- |
-- | Example:
-- |   data Status = Pending | Active | Completed
-- |   derive instance Generic Status _
-- |
-- |   type StatusParam = Enum Status
data Enum :: Type -> Type
data Enum a

--------------------------------------------------------------------------------
-- Metadata Extraction Typeclasses
--
-- Each Has* class matches its own wrapper and recurses through all others.
--------------------------------------------------------------------------------

-- Helper: all wrappers that a Has* class needs to recurse through.
-- When adding a new wrapper, add a recurse instance to every Has* class
-- and add the new Has* class with recurse instances for all existing wrappers.

class HasDescription :: Type -> Constraint
class HasDescription ty where
  description :: Proxy ty -> Maybe String

instance IsSymbol desc => HasDescription (Description desc a) where
  description _ = Just (reflectSymbol (Proxy :: Proxy desc))
else instance HasDescription a => HasDescription (Example ex a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (Format fmt a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (Minimum v a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (Maximum v a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (Pattern pat a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (MinLength v a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (MaxLength v a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (Title t a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (Nullable a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (Default val a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (Deprecated a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription a => HasDescription (Enum a) where
  description _ = description (Proxy :: Proxy a)
else instance HasDescription ty where
  description _ = Nothing

class HasExample :: Type -> Constraint
class HasExample ty where
  example :: Proxy ty -> Maybe String

instance IsSymbol ex => HasExample (Example ex a) where
  example _ = Just (reflectSymbol (Proxy :: Proxy ex))
else instance HasExample a => HasExample (Description desc a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (Format fmt a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (Minimum v a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (Maximum v a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (Pattern pat a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (MinLength v a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (MaxLength v a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (Title t a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (Nullable a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (Default val a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (Deprecated a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample a => HasExample (Enum a) where
  example _ = example (Proxy :: Proxy a)
else instance HasExample ty where
  example _ = Nothing

class HasFormat :: Type -> Constraint
class HasFormat ty where
  format :: Proxy ty -> Maybe String

instance IsSymbol fmt => HasFormat (Format fmt a) where
  format _ = Just (reflectSymbol (Proxy :: Proxy fmt))
else instance HasFormat a => HasFormat (Description desc a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (Example ex a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (Minimum v a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (Maximum v a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (Pattern pat a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (MinLength v a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (MaxLength v a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (Title t a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (Nullable a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (Default val a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (Deprecated a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat a => HasFormat (Enum a) where
  format _ = format (Proxy :: Proxy a)
else instance HasFormat ty where
  format _ = Nothing

class HasMinimum :: Type -> Constraint
class HasMinimum ty where
  minimum :: Proxy ty -> Maybe Int

instance Reflectable v Int => HasMinimum (Minimum v a) where
  minimum _ = Just (reflectType (Proxy :: Proxy v))
else instance HasMinimum a => HasMinimum (Description desc a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (Example ex a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (Format fmt a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (Maximum v a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (Pattern pat a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (MinLength v a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (MaxLength v a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (Title t a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (Nullable a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (Default val a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (Deprecated a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum a => HasMinimum (Enum a) where
  minimum _ = minimum (Proxy :: Proxy a)
else instance HasMinimum ty where
  minimum _ = Nothing

class HasMaximum :: Type -> Constraint
class HasMaximum ty where
  maximum :: Proxy ty -> Maybe Int

instance Reflectable v Int => HasMaximum (Maximum v a) where
  maximum _ = Just (reflectType (Proxy :: Proxy v))
else instance HasMaximum a => HasMaximum (Description desc a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (Example ex a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (Format fmt a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (Minimum v a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (Pattern pat a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (MinLength v a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (MaxLength v a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (Title t a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (Nullable a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (Default val a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (Deprecated a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum a => HasMaximum (Enum a) where
  maximum _ = maximum (Proxy :: Proxy a)
else instance HasMaximum ty where
  maximum _ = Nothing

class HasPattern :: Type -> Constraint
class HasPattern ty where
  pattern :: Proxy ty -> Maybe String

instance IsSymbol pat => HasPattern (Pattern pat a) where
  pattern _ = Just (reflectSymbol (Proxy :: Proxy pat))
else instance HasPattern a => HasPattern (Description desc a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (Example ex a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (Format fmt a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (Minimum v a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (Maximum v a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (MinLength v a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (MaxLength v a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (Title t a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (Nullable a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (Default val a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (Deprecated a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern a => HasPattern (Enum a) where
  pattern _ = pattern (Proxy :: Proxy a)
else instance HasPattern ty where
  pattern _ = Nothing

class HasMinLength :: Type -> Constraint
class HasMinLength ty where
  minLength :: Proxy ty -> Maybe Int

instance Reflectable v Int => HasMinLength (MinLength v a) where
  minLength _ = Just (reflectType (Proxy :: Proxy v))
else instance HasMinLength a => HasMinLength (Description desc a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Example ex a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Format fmt a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Minimum v a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Maximum v a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Pattern pat a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (MaxLength v a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Title t a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Nullable a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Default val a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Deprecated a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength a => HasMinLength (Enum a) where
  minLength _ = minLength (Proxy :: Proxy a)
else instance HasMinLength ty where
  minLength _ = Nothing

class HasMaxLength :: Type -> Constraint
class HasMaxLength ty where
  maxLength :: Proxy ty -> Maybe Int

instance Reflectable v Int => HasMaxLength (MaxLength v a) where
  maxLength _ = Just (reflectType (Proxy :: Proxy v))
else instance HasMaxLength a => HasMaxLength (Description desc a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Example ex a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Format fmt a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Minimum v a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Maximum v a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Pattern pat a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (MinLength v a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Title t a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Nullable a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Default val a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Deprecated a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength a => HasMaxLength (Enum a) where
  maxLength _ = maxLength (Proxy :: Proxy a)
else instance HasMaxLength ty where
  maxLength _ = Nothing

class HasTitle :: Type -> Constraint
class HasTitle ty where
  title :: Proxy ty -> Maybe String

instance IsSymbol t => HasTitle (Title t a) where
  title _ = Just (reflectSymbol (Proxy :: Proxy t))
else instance HasTitle a => HasTitle (Description desc a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (Example ex a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (Format fmt a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (Minimum v a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (Maximum v a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (Pattern pat a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (MinLength v a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (MaxLength v a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (Nullable a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (Default val a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (Deprecated a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle a => HasTitle (Enum a) where
  title _ = title (Proxy :: Proxy a)
else instance HasTitle ty where
  title _ = Nothing

class HasNullable :: Type -> Constraint
class HasNullable ty where
  nullable :: Proxy ty -> Boolean

instance HasNullable (Nullable a) where
  nullable _ = true
else instance HasNullable a => HasNullable (Description desc a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (Example ex a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (Format fmt a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (Minimum v a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (Maximum v a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (Pattern pat a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (MinLength v a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (MaxLength v a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (Title t a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (Default val a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (Deprecated a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable a => HasNullable (Enum a) where
  nullable _ = nullable (Proxy :: Proxy a)
else instance HasNullable ty where
  nullable _ = false

class HasDefault :: Type -> Constraint
class HasDefault ty where
  default :: Proxy ty -> Maybe String

instance IsSymbol val => HasDefault (Default val a) where
  default _ = Just (reflectSymbol (Proxy :: Proxy val))
else instance HasDefault a => HasDefault (Description desc a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (Example ex a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (Format fmt a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (Minimum v a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (Maximum v a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (Pattern pat a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (MinLength v a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (MaxLength v a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (Title t a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (Nullable a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (Deprecated a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault a => HasDefault (Enum a) where
  default _ = default (Proxy :: Proxy a)
else instance HasDefault ty where
  default _ = Nothing

class HasDeprecated :: Type -> Constraint
class HasDeprecated ty where
  deprecated :: Proxy ty -> Boolean

instance HasDeprecated (Deprecated a) where
  deprecated _ = true
else instance HasDeprecated a => HasDeprecated (Description desc a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (Example ex a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (Format fmt a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (Minimum v a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (Maximum v a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (Pattern pat a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (MinLength v a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (MaxLength v a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (Title t a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (Nullable a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (Default val a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated a => HasDeprecated (Enum a) where
  deprecated _ = deprecated (Proxy :: Proxy a)
else instance HasDeprecated ty where
  deprecated _ = false

class HasEnum :: Type -> Constraint
class HasEnum ty where
  enum :: Proxy ty -> Maybe (Array String)

-- | Extract enum values from a Generic sum type representation.
-- | Walks through GR.Sum constructors and collects constructor names.
class GenericEnumValues :: Type -> Constraint
class GenericEnumValues rep where
  genericEnumValues :: Proxy rep -> Array String

-- Walk sum types (left and right branches)
instance (GenericEnumValues a, GenericEnumValues b) => GenericEnumValues (GR.Sum a b) where
  genericEnumValues _ =
    genericEnumValues (Proxy :: Proxy a) <> genericEnumValues (Proxy :: Proxy b)

-- Extract constructor name from no-argument constructors
instance IsSymbol name => GenericEnumValues (GR.Constructor name GR.NoArguments) where
  genericEnumValues _ = [ reflectSymbol (Proxy :: Proxy name) ]

-- Main instance: extract enum values from a Generic type wrapped in Enum
instance (Generic a rep, GenericEnumValues rep) => HasEnum (Enum a) where
  enum _ = Just (genericEnumValues (Proxy :: Proxy rep))
else instance HasEnum a => HasEnum (Description desc a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (Example ex a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (Format fmt a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (Minimum v a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (Maximum v a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (Pattern pat a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (MinLength v a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (MaxLength v a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (Title t a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (Nullable a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (Default val a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum a => HasEnum (Deprecated a) where
  enum _ = enum (Proxy :: Proxy a)
else instance HasEnum ty where
  enum _ = Nothing

--------------------------------------------------------------------------------
-- Operation-Level Metadata
--------------------------------------------------------------------------------

type OperationMetadata =
  { summary :: Maybe String
  , description :: Maybe String
  , operationId :: Maybe String
  , tags :: Array String
  , deprecated :: Boolean
  }

class HasOperationMetadata :: Type -> Constraint
class HasOperationMetadata route where
  operationMetadata :: Proxy route -> OperationMetadata

instance (Warn (Text "Using default operation metadata")) => HasOperationMetadata route where
  operationMetadata _ =
    { summary: Nothing
    , description: Nothing
    , operationId: Nothing
    , tags: []
    , deprecated: false
    }

--------------------------------------------------------------------------------
-- Metadata Wrapper Transparency (Runtime)
--
-- All wrappers are phantom types — transparent at runtime via unsafeCoerce.
--------------------------------------------------------------------------------

-- HeaderValue instances
instance HeaderValue a => HeaderValue (Description desc a) where
  parseHeader s = unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance HeaderValue a => HeaderValue (Example ex a) where
  parseHeader s = unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance HeaderValue a => HeaderValue (Format fmt a) where
  parseHeader s = unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance (Reflectable v Int, HeaderValue a) => HeaderValue (Minimum v a) where
  parseHeader s = case (unsafeCoerce :: Either String a -> Either String Int) (parseHeader s :: Either String a) of
    Left err -> Left err
    Right val ->
      let
        minVal = reflectType (Proxy :: Proxy v)
      in
        if val < minVal then Left ("Value " <> show val <> " is less than minimum " <> show minVal)
        else Right (unsafeCoerce val)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance (Reflectable v Int, HeaderValue a) => HeaderValue (Maximum v a) where
  parseHeader s = case (unsafeCoerce :: Either String a -> Either String Int) (parseHeader s :: Either String a) of
    Left err -> Left err
    Right val ->
      let
        maxVal = reflectType (Proxy :: Proxy v)
      in
        if val > maxVal then Left ("Value " <> show val <> " exceeds maximum " <> show maxVal)
        else Right (unsafeCoerce val)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance (IsSymbol pat, HeaderValue a) => HeaderValue (Pattern pat a) where
  parseHeader s = case Regex.regex (reflectSymbol (Proxy :: Proxy pat)) noFlags of
    Left err -> Left ("Invalid regex pattern: " <> err)
    Right re ->
      if Regex.test re s then unsafeCoerce (parseHeader s :: Either String a)
      else Left ("Value '" <> s <> "' does not match pattern: " <> reflectSymbol (Proxy :: Proxy pat))
  printHeader x = printHeader (unsafeCoerce x :: a)

instance (Reflectable v Int, HeaderValue a) => HeaderValue (MinLength v a) where
  parseHeader s =
    let
      minLen = reflectType (Proxy :: Proxy v)
    in
      if SCU.length s < minLen then Left ("String length " <> show (SCU.length s) <> " is less than minimum " <> show minLen)
      else unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance (Reflectable v Int, HeaderValue a) => HeaderValue (MaxLength v a) where
  parseHeader s =
    let
      maxLen = reflectType (Proxy :: Proxy v)
    in
      if SCU.length s > maxLen then Left ("String length " <> show (SCU.length s) <> " exceeds maximum " <> show maxLen)
      else unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance HeaderValue a => HeaderValue (Title t a) where
  parseHeader s = unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance HeaderValue a => HeaderValue (Nullable a) where
  parseHeader s = unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance HeaderValue a => HeaderValue (Default val a) where
  parseHeader s = unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance HeaderValue a => HeaderValue (Deprecated a) where
  parseHeader s = unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

instance HeaderValue a => HeaderValue (Enum a) where
  parseHeader s = unsafeCoerce (parseHeader s :: Either String a)
  printHeader x = printHeader (unsafeCoerce x :: a)

-- HeaderValueType instances
instance HeaderValueType a => HeaderValueType (Description desc a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Example ex a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Format fmt a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Minimum v a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Maximum v a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Pattern pat a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (MinLength v a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (MaxLength v a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Title t a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Nullable a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Default val a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Deprecated a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

instance HeaderValueType a => HeaderValueType (Enum a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)

-- ParseParam instances
instance ParseParam a => ParseParam (Description desc a) where
  parseParam s = unsafeCoerce (parseParam s :: Either String a)

instance ParseParam a => ParseParam (Example ex a) where
  parseParam s = unsafeCoerce (parseParam s :: Either String a)

instance ParseParam a => ParseParam (Format fmt a) where
  parseParam s = unsafeCoerce (parseParam s :: Either String a)

instance (Reflectable v Int, ParseParam a) => ParseParam (Minimum v a) where
  parseParam s = case (unsafeCoerce :: Either String a -> Either String Int) (parseParam s :: Either String a) of
    Left err -> Left err
    Right val ->
      let
        minVal = reflectType (Proxy :: Proxy v)
      in
        if val < minVal then Left ("Value " <> show val <> " is less than minimum " <> show minVal)
        else Right (unsafeCoerce val)

instance (Reflectable v Int, ParseParam a) => ParseParam (Maximum v a) where
  parseParam s = case (unsafeCoerce :: Either String a -> Either String Int) (parseParam s :: Either String a) of
    Left err -> Left err
    Right val ->
      let
        maxVal = reflectType (Proxy :: Proxy v)
      in
        if val > maxVal then Left ("Value " <> show val <> " exceeds maximum " <> show maxVal)
        else Right (unsafeCoerce val)

instance (IsSymbol pat, ParseParam a) => ParseParam (Pattern pat a) where
  parseParam s = case Regex.regex (reflectSymbol (Proxy :: Proxy pat)) noFlags of
    Left err -> Left ("Invalid regex pattern: " <> err)
    Right re ->
      if Regex.test re s then unsafeCoerce (parseParam s :: Either String a)
      else Left ("Value '" <> s <> "' does not match pattern: " <> reflectSymbol (Proxy :: Proxy pat))

instance (Reflectable v Int, ParseParam a) => ParseParam (MinLength v a) where
  parseParam s =
    let
      minLen = reflectType (Proxy :: Proxy v)
    in
      if SCU.length s < minLen then Left ("String length " <> show (SCU.length s) <> " is less than minimum " <> show minLen)
      else unsafeCoerce (parseParam s :: Either String a)

instance (Reflectable v Int, ParseParam a) => ParseParam (MaxLength v a) where
  parseParam s =
    let
      maxLen = reflectType (Proxy :: Proxy v)
    in
      if SCU.length s > maxLen then Left ("String length " <> show (SCU.length s) <> " exceeds maximum " <> show maxLen)
      else unsafeCoerce (parseParam s :: Either String a)

instance ParseParam a => ParseParam (Title t a) where
  parseParam s = unsafeCoerce (parseParam s :: Either String a)

instance ParseParam a => ParseParam (Nullable a) where
  parseParam s = unsafeCoerce (parseParam s :: Either String a)

instance ParseParam a => ParseParam (Default val a) where
  parseParam s = unsafeCoerce (parseParam s :: Either String a)

instance ParseParam a => ParseParam (Deprecated a) where
  parseParam s = unsafeCoerce (parseParam s :: Either String a)

instance ParseParam a => ParseParam (Enum a) where
  parseParam s = unsafeCoerce (parseParam s :: Either String a)

-- WriteForeign instances
instance WriteForeign a => WriteForeign (Description desc a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Example ex a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Format fmt a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Minimum v a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Maximum v a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Pattern pat a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (MinLength v a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (MaxLength v a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Title t a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Nullable a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Default val a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Deprecated a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

instance WriteForeign a => WriteForeign (Enum a) where
  writeImpl x = writeImpl (unsafeCoerce x :: a)

-- ReadForeign instances
instance ReadForeign a => ReadForeign (Description desc a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Example ex a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Format fmt a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Minimum v a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Maximum v a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Pattern pat a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (MinLength v a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (MaxLength v a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Title t a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Nullable a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Default val a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Deprecated a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)

instance ReadForeign a => ReadForeign (Enum a) where
  readImpl = unsafeCoerce (readImpl :: Foreign -> _ a)
