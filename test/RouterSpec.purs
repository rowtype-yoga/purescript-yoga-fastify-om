module Test.Yoga.Fastify.Om.RouterSpec where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Fastify.Fastify (RouteURL(..))
import Yoga.Fastify.Om.Router as Router

-- | Example route type for a blog application
data BlogRoute
  = Home
  | About
  | Posts
  | Post Int
  | UserProfile String

derive instance Generic BlogRoute _
derive instance Eq BlogRoute

instance Show BlogRoute where
  show Home = "Home"
  show About = "About"
  show Posts = "Posts"
  show (Post n) = "Post " <> show n
  show (UserProfile s) = "UserProfile " <> show s

-- | Route codec using routing-duplex
blogRoute :: RouteDuplex' BlogRoute
blogRoute = RD.root $ RG.sum
  { "Home": RG.noArgs
  , "About": "about" / RG.noArgs
  , "Posts": "posts" / RG.noArgs
  , "Post": "posts" / RD.int RD.segment
  , "UserProfile": "users" / RD.string RD.segment
  }

spec :: Spec Unit
spec = do
  describe "routing-duplex Integration" do

    describe "Route Parsing" do
      it "parses home route" do
        Router.matchRouteUrl blogRoute (RouteURL "/") `shouldEqual` Right Home

      it "parses about route" do
        Router.matchRouteUrl blogRoute (RouteURL "/about") `shouldEqual` Right About

      it "parses posts list route" do
        Router.matchRouteUrl blogRoute (RouteURL "/posts") `shouldEqual` Right Posts

      it "parses single post route" do
        Router.matchRouteUrl blogRoute (RouteURL "/posts/123") `shouldEqual` Right (Post 123)

      it "parses user profile route" do
        Router.matchRouteUrl blogRoute (RouteURL "/users/alice")
          `shouldEqual` Right (UserProfile "alice")

    describe "Route Printing" do
      it "prints home route" do
        RD.print blogRoute Home `shouldEqual` "/"

      it "prints about route" do
        RD.print blogRoute About `shouldEqual` "/about"

      it "prints post route" do
        RD.print blogRoute (Post 123) `shouldEqual` "/posts/123"

      it "prints user profile route" do
        RD.print blogRoute (UserProfile "alice") `shouldEqual` "/users/alice"

    describe "Bidirectional (Parse → Print → Parse)" do
      it "round-trips post route" do
        let
          route = Post 123
          url = RD.print blogRoute route
          parsed = RD.parse blogRoute url
        parsed `shouldEqual` Right route

      it "round-trips user profile route" do
        let
          route = UserProfile "bob"
          url = RD.print blogRoute route
          parsed = RD.parse blogRoute url
        parsed `shouldEqual` Right route
