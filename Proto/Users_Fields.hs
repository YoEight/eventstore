{- This file was auto-generated from users.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Users_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
currentPassword ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "currentPassword" a) =>
  Lens.Family2.LensLike' f s a
currentPassword = Data.ProtoLens.Field.field @"currentPassword"
disabled ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "disabled" a) =>
  Lens.Family2.LensLike' f s a
disabled = Data.ProtoLens.Field.field @"disabled"
fullName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fullName" a) =>
  Lens.Family2.LensLike' f s a
fullName = Data.ProtoLens.Field.field @"fullName"
groups ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "groups" a) =>
  Lens.Family2.LensLike' f s a
groups = Data.ProtoLens.Field.field @"groups"
lastUpdated ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "lastUpdated" a) =>
  Lens.Family2.LensLike' f s a
lastUpdated = Data.ProtoLens.Field.field @"lastUpdated"
loginName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "loginName" a) =>
  Lens.Family2.LensLike' f s a
loginName = Data.ProtoLens.Field.field @"loginName"
maybe'lastUpdated ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'lastUpdated" a) =>
  Lens.Family2.LensLike' f s a
maybe'lastUpdated = Data.ProtoLens.Field.field @"maybe'lastUpdated"
maybe'options ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'options" a) =>
  Lens.Family2.LensLike' f s a
maybe'options = Data.ProtoLens.Field.field @"maybe'options"
maybe'userDetails ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'userDetails" a) =>
  Lens.Family2.LensLike' f s a
maybe'userDetails = Data.ProtoLens.Field.field @"maybe'userDetails"
newPassword ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "newPassword" a) =>
  Lens.Family2.LensLike' f s a
newPassword = Data.ProtoLens.Field.field @"newPassword"
options ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "options" a) =>
  Lens.Family2.LensLike' f s a
options = Data.ProtoLens.Field.field @"options"
password ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "password" a) =>
  Lens.Family2.LensLike' f s a
password = Data.ProtoLens.Field.field @"password"
ticksSinceEpoch ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ticksSinceEpoch" a) =>
  Lens.Family2.LensLike' f s a
ticksSinceEpoch = Data.ProtoLens.Field.field @"ticksSinceEpoch"
userDetails ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "userDetails" a) =>
  Lens.Family2.LensLike' f s a
userDetails = Data.ProtoLens.Field.field @"userDetails"
vec'groups ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'groups" a) =>
  Lens.Family2.LensLike' f s a
vec'groups = Data.ProtoLens.Field.field @"vec'groups"