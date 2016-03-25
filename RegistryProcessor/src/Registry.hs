-- This module contains a 1:1 translation of registry.rnc with an (un-)parser.
-- No simplifications are done here.
module Registry (
  parseRegistry, unparseRegistry,
  Registry(..),
  RegistryElement(..),
  VendorIds(..),
  VendorId(..),
  Tags(..),
  Tag(..),
  Types(..),
  Type(..),
  Enums(..),
  Enum'(..),
  Unused(..),
  Commands(..),
  Command(..),
  Proto(..),
  Param(..),
  GLX(..),
  Feature(..),
  Modification(..),
  ModificationKind(..),
  Extensions(..),
  Extension(..),
  ConditionalModification(..),
  InterfaceElement(..),
  InterfaceElementKind(..),
  Integer'(..),
  TypeName(..),
  TypeSuffix(..),
  StringGroup(..),
  ProfileName(..),
  Vendor(..),
  Comment(..),
  Name(..),
  Author(..),
  Contact(..),
  Bool'(..)
) where

import Data.Maybe ( maybeToList )
import Text.XML.HXT.Core

parseRegistry :: String -> Either String Registry
parseRegistry = head . (runLA $
  xreadDoc >>>
  neg isXmlPi >>>
  removeAllWhiteSpace >>>
  canonicalizeAllNodes  >>> -- needed to e.g. process CDATA, remove XML comments, etc.
  arr (unpickleDoc' xpRegistry))

unparseRegistry :: Registry -> String
unparseRegistry =
  concat . (pickleDoc xpRegistry >>>
            runLA (writeDocumentToString [withIndent yes,
                                          withOutputEncoding utf8,
                                          withXmlPi yes]))

-- Note: We do this slightly different from the schema.
newtype Registry = Registry {
  unRegistry :: [RegistryElement]
  } deriving (Eq, Ord, Show)

xpRegistry :: PU Registry
xpRegistry =
  xpWrap (Registry, unRegistry) $
  xpElem "registry" $
  xpList xpRegistryElement

data RegistryElement
  = CommentElement { unCommentElement :: Comment }
  | VendorIdsElement { unVendorIdsElement :: VendorIds }
  | TagsElement { unTagsElement :: Tags }
  | TypesElement { unTypesElement:: Types }
  | EnumsElement { unEnumsElement :: Enums }
  | CommandsElement { unCommandsElement :: Commands }
  | FeatureElement { unFeatureElement :: Feature }
  | ExtensionsElement { unExtensionsElement :: Extensions }
  deriving (Eq, Ord, Show)

xpRegistryElement :: PU RegistryElement
xpRegistryElement = xpAlt tag pus
  where tag (CommentElement _) = 0
        tag (VendorIdsElement _) = 1
        tag (TagsElement _) = 2
        tag (TypesElement _) = 3
        tag (EnumsElement _) = 4
        tag (CommandsElement _) = 5
        tag (FeatureElement _) = 6
        tag (ExtensionsElement _) = 7
        pus = [ xpWrap (CommentElement, unCommentElement) xpCommentElement
              , xpWrap (VendorIdsElement, unVendorIdsElement) xpVendorIds
              , xpWrap (TagsElement, unTagsElement) xpTags
              , xpWrap (TypesElement, unTypesElement) xpTypes
              , xpWrap (EnumsElement, unEnumsElement) xpEnums
              , xpWrap (CommandsElement, unCommandsElement) xpCommands
              , xpWrap (FeatureElement, unFeatureElement) xpFeature
              , xpWrap (ExtensionsElement, unExtensionsElement) xpExtensions ]

xpCommentElement :: PU Comment
xpCommentElement =
  xpWrap (Comment, unComment) $
  xpElem "comment" xpText

newtype VendorIds = VendorIds {
  unVendorIds :: [VendorId]
  } deriving (Eq, Ord, Show)

xpVendorIds :: PU VendorIds
xpVendorIds =
  xpWrap (VendorIds, unVendorIds) $
  xpElem "vendorids" $
  xpList xpVendorId

data VendorId = VendorId {
  vendorIdName :: Vendor,
  vendorIdId :: Integer',
  vendorIdComment :: Maybe Comment
  } deriving (Eq, Ord, Show)

xpVendorId :: PU VendorId
xpVendorId =
  xpWrap (\(a,b,c) -> VendorId a b c
         ,\(VendorId a b c) -> (a,b,c)) $
  xpElem "vendorid" $
  xpTriple
    (xpWrap (Vendor, unVendor) (xpAttr "name" xpText))
    (xpAttr "id" xpInteger)
    (xpOption xpComment)

newtype Tags = Tags {
  unTags :: [Tag]
  } deriving (Eq, Ord, Show)

xpTags :: PU Tags
xpTags =
  xpWrap (Tags, unTags) $
  xpElem "tags" $
  xpList xpTag

data Tag = Tag {
  tagName :: Vendor,
  tagAuthor :: Author,
  tagContact :: Contact
  } deriving (Eq, Ord, Show)

xpTag :: PU Tag
xpTag =
  xpWrap (\(a,b,c) -> Tag a b c
         ,\(Tag a b c) -> (a,b,c)) $
  xpElem "tag" $
  xpTriple
    (xpWrap (Vendor, unVendor) (xpAttr "name" xpText))
    (xpAttr "author" xpAuthor)
    (xpAttr "contact" xpContact)

newtype Types = Types {
  unTypes :: [Type]
  } deriving (Eq, Ord, Show)

xpTypes :: PU Types
xpTypes =
  xpWrap (Types, unTypes) $
  xpElem "types" $
  xpList xpType

data Type = Type {
  typeAPI :: Maybe String,
  typeRequires :: Maybe String,
  typeName1 :: Maybe TypeName,
  typeCategory :: Maybe String,
  typeParent :: Maybe TypeName,
  typeReturnedOnly :: Maybe Bool',
  typeComment :: Maybe Comment,
  -- TODO: Fields below are bogus for Vulkan
  typeType :: Maybe String,
  typeText1 :: String,
  typeAPIEntry :: Maybe String,
  typeText2 :: String,
  typeName2 :: Maybe TypeName,
  typeText3 :: String
  } deriving (Eq, Ord, Show)

xpType :: PU Type
xpType =
  xpWrap (\(a,b,c,d,e,f,g,h,i,j,k,l,m) -> Type a b c d e f g h i j k l m
         ,\(Type a b c d e f g h i j k l m) -> (a,b,c,d,e,f,g,h,i,j,k,l,m)) $
  xpElem "type" $
  xp13Tuple
    (xpAttrImplied "api" xpText)
    (xpAttrImplied "requires" xpText)
    (xpAttrImplied "name" xpTypeName)
    (xpAttrImplied "category" xpText)
    (xpAttrImplied "parent" xpTypeName)
    (xpAttrImplied "returnedonly" xpBool)
    (xpOption xpComment)
    (xpAttrImplied "type" xpText)
    xpText0
    (xpOption $ xpElem "apientry" xpText0)
    xpText0
    (xpOption $ xpElem "name" xpTypeName)
    xpText0

data Enums = Enums {
  enumsName :: Maybe TypeName,
  enumsType :: Maybe String,
  enumsStart :: Maybe Integer',
  enumsEnd :: Maybe Integer',
  enumsExpand :: Maybe String,
  enumsVendor :: Maybe Vendor,
  enumsComment :: Maybe Comment,
  enumsEnumOrUnuseds :: [Either Enum' Unused]
  } deriving (Eq, Ord, Show)

xpEnums :: PU Enums
xpEnums =
  xpWrap (\(a,b,c,d,e,f,g,h) -> Enums a b c d e f g h
         ,\(Enums a b c d e f g h) -> (a,b,c,d,e,f,g,h)) $
  xpElem "enums" $
  xp8Tuple
    (xpAttrImplied "name" xpTypeName)
    (xpAttrImplied "type" xpText)
    (xpAttrImplied "start" xpInteger)
    (xpAttrImplied "end" xpInteger)
    (xpAttrImplied "expand" xpText)
    (xpOption xpVendor)
    (xpOption xpComment)
    (xpList $ xpEither xpEnum xpUnused)

xpEither :: PU a -> PU b -> PU (Either a b)
xpEither pl pr = xpAlt tag pus
  where tag (Left _) = 0
        tag (Right _) = 1
        pus = [ xpWrap (Left, \(Left l) -> l) pl
              , xpWrap (Right, \(Right r) -> r) pr ]

data Enum' = Enum {
  enumValue :: String,  -- TODO: 3 possibilities for Vulkan
  enumAPI :: Maybe String,
  enumType :: Maybe TypeSuffix,
  enumName :: String,
  enumAlias :: Maybe String,
  enumComment :: Maybe Comment
  } deriving (Eq, Ord, Show)

-- NOTE: The spec uses the interleave pattern, which is not needed: Attributes
-- are by definition unordered.
xpEnum :: PU Enum'
xpEnum =
  xpWrap (\(a,b,c,d,e,f) -> Enum a b c d e f
         ,\(Enum a b c d e f) -> (a,b,c,d,e,f)) $
  xpElem "enum" $
  xp6Tuple
    (xpAttr "value" xpText)
    (xpAttrImplied "api" xpText)
    (xpAttrImplied "type" xpTypeSuffix)
    (xpAttr "name" xpText)
    (xpAttrImplied "alias" xpText)
    (xpOption xpComment)

data Unused = Unused {
  unusedStart :: Integer',
  unusedEnd :: Maybe Integer',
  unusedVendor :: Maybe Vendor,
  unusedComment :: Maybe Comment
  } deriving (Eq, Ord, Show)

xpUnused :: PU Unused
xpUnused =
  xpWrap (\(a,b,c,d) -> Unused a b c d
         ,\(Unused a b c d) -> (a,b,c,d)) $
  xpElem "unused" $
  xp4Tuple
    (xpAttr "start" xpInteger)
    (xpAttrImplied "end" xpInteger)
    (xpOption xpVendor)
    (xpOption xpComment)

newtype Commands = Commands {
  unCommands :: [Command]
  } deriving (Eq, Ord, Show)

xpCommands :: PU Commands
xpCommands =
  xpWrap (Commands, unCommands) $
  xpElem "commands" $
  xpList xpCommand

data Command = Command {
  commandComment :: Maybe Comment,
  commandProto :: Proto,
  commandParams :: [Param],
  commandAlias :: Maybe Name,
  commandVecEquiv :: Maybe Name,
  commandGLXs :: [GLX]
  } deriving (Eq, Ord, Show)

xpCommand :: PU Command
xpCommand =
  xpWrap (\(a,b,c,(d,e,f)) -> Command a b c d e f
         ,\(Command a b c d e f) -> (a,b,c,(d,e,f))) $
  xpElem "command" $
  xp4Tuple
    (xpOption xpComment)
    xpProto
    (xpList xpParam)
    xpCommandTail

-- The spec uses the interleave pattern here, which is not supported in hxt.
-- As a workaround, we use a list of disjoint types.
xpCommandTail :: PU (Maybe Name, Maybe Name, [GLX])
xpCommandTail =
  xpWrapEither (\xs -> do a <- check "alias" [x | AliasElement x <- xs]
                          b <- check "vecequiv" [x | VecEquivElement x <- xs]
                          c <- return [x | GLXElement x <- xs]
                          return (a,b,c)
               ,\(a,b,c) -> map AliasElement (maybeToList a) ++
                            map VecEquivElement (maybeToList b) ++
                            map GLXElement c) $
  xpList $
  xpAlt tag pus
    where tag (AliasElement _) = 0
          tag (VecEquivElement _) = 1
          tag (GLXElement _) = 2
          pus = [xpWrap (AliasElement,unAliasElement) $ xpElem "alias" xpName
                ,xpWrap (VecEquivElement,unVecEquivElement) $ xpElem "vecequiv" xpName
                ,xpWrap (GLXElement,unGLXElement) xpGLX]
          check n xs = case xs of
                       [] -> Right Nothing
                       [x] -> Right $ Just x
                       _ -> Left $ "expected at most one '" ++ n ++ "' element"

data CommandTail
  = AliasElement { unAliasElement :: Name }
  | VecEquivElement { unVecEquivElement :: Name }
  | GLXElement { unGLXElement :: GLX }
  deriving (Eq, Ord, Show)

data Proto = Proto {
  protoGroup :: Maybe String,
  protoText1 :: String,
  protoPtype :: Maybe TypeName,
  protoText2 :: String,
  protoName :: String,
  protoText3 :: String
  } deriving (Eq, Ord, Show)

xpProto :: PU Proto
xpProto =
  xpWrap (\(a,b,c,d,e,f) -> Proto a b c d e f
         ,\(Proto a b c d e f) -> (a,b,c,d,e,f)) $
  xpElem "proto" $
  xp6Tuple
    (xpAttrImplied "group" xpText)
    xpText0
    (xpOption $ xpElem "ptype" xpTypeName)
    xpText0
    (xpElem "name" xpText)
    xpText0

data Param = Param {
  paramLen :: Maybe String,
  paramProto :: Proto
  } deriving (Eq, Ord, Show)

xpParam :: PU Param
xpParam =
  xpWrap (\(b,a,c,d,e,f,g) -> Param a (Proto b c d e f g)
         ,\(Param a (Proto b c d e f g)) -> (b,a,c,d,e,f,g)) $
  xpElem "param" $
  xp7Tuple
    (xpAttrImplied "group" xpText)
    (xpAttrImplied "len" xpText)
    xpText0
    (xpOption $ xpElem "ptype" xpTypeName)
    xpText0
    (xpElem "name" xpText)
    xpText0

data GLX = GLX {
  glxType :: String,
  glxOpcode :: Int,
  glxName :: Maybe Name,
  glxComment :: Maybe Comment
  } deriving (Eq, Ord, Show)

xpGLX :: PU GLX
xpGLX =
  xpWrap (\(a,b,c,d) -> GLX a b c d
         ,\(GLX a b c d) -> (a,b,c,d)) $
  xpElem "glx" $
  xp4Tuple
    (xpAttr "type" xpText)
    (xpAttr "opcode" xpInt)
    (xpOption xpName)
    (xpOption xpComment)

data Feature = Feature {
  featureAPI :: String,
  featureName :: Name,
  featureNumber :: String,  -- actually xsd:decimal, but used as a string
  featureProtect :: Maybe String,
  featureComment :: Maybe Comment,
  featureModifications :: [Modification]
  } deriving (Eq, Ord, Show)

xpFeature :: PU Feature
xpFeature =
  xpWrap (\(a,b,c,d,e,f) -> Feature a b c d e f
         ,\(Feature a b c d e f) -> (a,b,c,d,e,f)) $
  xpElem "feature" $
  xp6Tuple
    (xpAttr "api" xpText)
    xpName
    (xpAttr "number" xpText)
    (xpAttrImplied "protect" xpText)
    (xpOption xpComment)
    (xpList xpModification)

data Modification = Modification {
  modificationModificationKind :: ModificationKind,
  modificationProfileName :: Maybe ProfileName,
  modificationComment :: Maybe Comment,
  modificationInterfaceElements :: [InterfaceElement]
  } deriving (Eq, Ord, Show)

data ModificationKind = Require | Remove  -- TODO: Better name
  deriving (Eq, Ord, Show, Enum)

xpModification :: PU Modification
xpModification =
  xpAlt (fromEnum . modificationModificationKind) pus
  where pus = [ xpMod "require" Require
              , xpMod "remove" Remove ]
        xpMod el kind =
          xpWrap (\(a,b,c) -> Modification kind a b c
                 ,\(Modification _ a b c) -> (a,b,c)) $
          xpElem el $
          xpTriple
            (xpOption xpProfileName)
            (xpOption xpComment)
            (xpList xpInterfaceElement)

newtype Extensions = Extensions {
  unExtensions :: [Extension]
  } deriving (Eq, Ord, Show)

xpExtensions :: PU Extensions
xpExtensions =
  xpWrap (Extensions, unExtensions) $
  xpElem "extensions" $
  xpList xpExtension

data Extension = Extension {
  extensionName :: Name,
  extensionNumber :: Maybe Integer',
  extensionProtect :: Maybe String,
  extensionSupported :: Maybe StringGroup,
  extensionAuthor :: Maybe Author,
  extensionContact :: Maybe Contact,
  extensionComment :: Maybe Comment,
  extensionsRequireRemove :: [ConditionalModification]
  } deriving (Eq, Ord, Show)

xpExtension :: PU Extension
xpExtension =
  xpWrap (\(a,b,c,d,e,f,g,h) -> Extension a b c d e f g h
         ,\(Extension a b c d e f g h) -> (a,b,c,d,e,f,g,h)) $
  xpElem "extension" $
  xp8Tuple
    xpName
    (xpAttrImplied "number" xpInteger)
    (xpAttrImplied "protect" xpText)
    (xpAttrImplied "supported" xpStringGroup)
    (xpAttrImplied "author" xpAuthor)
    (xpAttrImplied "contact" xpContact)
    (xpOption xpComment)
    (xpList xpConditionalModification)

data ConditionalModification = ConditionalModification {
  conditionalModificationAPI :: Maybe String,
  conditionalModificationModification :: Modification
  } deriving (Eq, Ord, Show)

xpConditionalModification :: PU ConditionalModification
xpConditionalModification =
  xpAlt (fromEnum . modificationModificationKind . conditionalModificationModification) pus
  where pus = [ xpMod "require" Require
              , xpMod "remove" Remove ]
        xpMod el kind =
          xpWrap (\(a,b,c,d) -> ConditionalModification a (Modification kind b c d)
                 ,\(ConditionalModification a (Modification _ b c d)) -> (a,b,c,d)) $
          xpElem el $
          xp4Tuple
            (xpAttrImplied "api" xpText)
            (xpOption xpProfileName)
            (xpOption xpComment)
            (xpList xpInterfaceElement)

data InterfaceElement = InterfaceElement {
   interfaceElementKind :: InterfaceElementKind,
   interfaceElementName :: Name,
   interfaceElementComment :: Maybe Comment
  } deriving (Eq, Ord, Show)

data InterfaceElementKind
  = InterfaceElementType
  | InterfaceElementEnum
  | InterfaceElementCommand
  deriving (Eq, Ord, Show, Enum)

xpInterfaceElement :: PU InterfaceElement
xpInterfaceElement = xpAlt (fromEnum . interfaceElementKind) pus
  where pus = [ xpIE InterfaceElementType "type"
              , xpIE InterfaceElementEnum "enum"
              , xpIE InterfaceElementCommand "command" ]
        xpIE ty el =
          xpWrap (\(a,b) -> InterfaceElement ty a b
                 ,\(InterfaceElement _ a b) -> (a,b)) $
          xpElem el $
          xpPair
            xpName
            (xpOption xpComment)

newtype Integer' = Integer { unInteger :: String } deriving (Eq, Ord, Show)

xpInteger :: PU Integer'
xpInteger = xpWrap (Integer, unInteger) xpText

newtype Author = Author { unAuthor :: String } deriving (Eq, Ord, Show)

newtype TypeName = TypeName { unTypeName :: String } deriving (Eq, Ord, Show)

xpTypeName :: PU TypeName
xpTypeName = xpWrap (TypeName, unTypeName) xpText

newtype TypeSuffix = TypeSuffix { unTypeSuffix :: String } deriving (Eq, Ord, Show)

xpTypeSuffix :: PU TypeSuffix
xpTypeSuffix = xpWrap (TypeSuffix, unTypeSuffix) xpText

newtype StringGroup = StringGroup { unStringGroup :: String } deriving (Eq, Ord, Show)

xpStringGroup :: PU StringGroup
xpStringGroup = xpWrap (StringGroup, unStringGroup) xpText

newtype ProfileName = ProfileName { unProfileName :: String } deriving (Eq, Ord, Show)

xpProfileName :: PU ProfileName
xpProfileName =
  xpWrap (ProfileName, unProfileName) $
  xpAttr "profile" xpText

newtype Vendor = Vendor { unVendor :: String } deriving (Eq, Ord, Show)

xpVendor :: PU Vendor
xpVendor =
  xpWrap (Vendor, unVendor) $
  xpAttr "vendor" xpText

newtype Comment = Comment { unComment :: String } deriving (Eq, Ord, Show)

xpComment :: PU Comment
xpComment =
  xpAttr "comment" $
  xpWrap (Comment, unComment) xpText

newtype Name = Name { unName :: String } deriving (Eq, Ord, Show)

xpName :: PU Name
xpName =
  xpWrap (Name, unName) $
  xpAttr "name" xpText

xpAuthor :: PU Author
xpAuthor = xpWrap (Author, unAuthor) xpText

newtype Contact = Contact { unContact :: String } deriving (Eq, Ord, Show)

xpContact :: PU Contact
xpContact = xpWrap (Contact, unContact) xpText

newtype Bool' = Bool { unBool :: String } deriving (Eq, Ord, Show)

xpBool :: PU Bool'
xpBool = xpWrap (Bool, unBool) xpText
