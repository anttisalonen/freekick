module Freekick.Libsoccer.Database (parsePitchXML, parseCountryXML, parsePlayerXML, parseClubXML)
where

import Libaddutil.XMLParse
import Freekick.Libsoccer.Pitch
import Freekick.Libsoccer.Country
import Freekick.Libsoccer.LeagueSystem
import Freekick.Libsoccer.Region
import Freekick.Libsoccer.Stage
import Freekick.Libsoccer.Trophy
import Freekick.Libsoccer.Player
import Libaddutil.Person
import Data.Maybe
import Libaddutil.Primitives
import Freekick.Libsoccer.DatabaseHelper
import Freekick.Libsoccer.Stadium

import Freekick.Libsoccer.Club

parsePitchXML :: FilePath -> IO [Pitch]
parsePitchXML file = parseXMLResource file "Pitches" parsePitch

parseXMLResource :: FilePath -> String -> (Node -> a) -> IO [a]
parseXMLResource file n f = do
    mr <- parseXml file
    if isNothing mr
      then return []
      else if eName (fromJust mr) /= n
          then return []
          else return $ reverse $ map f (eChildren (fromJust mr))

parseXMLNode :: (Node -> a) -> Node -> [a]
parseXMLNode f n = map f (eChildren n)

parsePitch :: Node -> Pitch
parsePitch n = Pitch entname entid frict (parseArea sizenode) (parsePoint areanode) imagename
    where entname      = fromMaybe "" (lookup "name" (eAttrs n))
          entid        = read $ fromMaybe "-1" (lookup "id" (eAttrs n))
          frict        = read $ fromMaybe "0"  (lookup "value" (eAttrs frictionnode))
          imagename    = fromMaybe "" (lookup "file" (eAttrs imagenode))
          frictionnode = fromMaybe nullNode (findNodeByName (eChildren n) "friction")
          sizenode     = fromMaybe nullNode (findNodeByName (eChildren n) "size")
          areanode     = fromMaybe nullNode (findNodeByName (eChildren n) "area")
          imagenode    = fromMaybe nullNode (findNodeByName (eChildren n) "image")

parseCountryXML :: FilePath -> IO [Country]
parseCountryXML file = parseXMLResource file "Countries" parseCountry

parseCountry :: Node -> Country
parseCountry n = Country entname abbr leaguesys regs natteam tourns
    where entname      = fromMaybe "" (lookup "name" (eAttrs n))
          abbr         = fromMaybe "" (lookup "abbr" (eAttrs n))
          regs         = parseXMLNode parseRegion (fromMaybe nullNode regionsnode)
          natteam      = read $ fromMaybe "-1" (lookup "id" (eAttrs (fromMaybe nullNode teamnode)))
          leaguesys    = parseLeagueSystem (fromMaybe nullNode leaguenode)
          tourns       = parseTournamentXML' (fromMaybe nullNode tournnode)
          regionsnode  = findNodeByName (eChildren n) "Regions"
          teamnode     = findNodeByName (eChildren n) "team"
          leaguenode   = findNodeByName (eChildren n) "leaguesystem"
          tournnode    = findNodeByName (eChildren n) "Tournaments"

parseRegion :: Node -> Region
parseRegion n = Region entname stads subregs
    where entname    = fromMaybe "" (lookup "name" (eAttrs n))
          stads      = map parseStadium (eChildren n)
          subregs    = map parseRegion (eChildren n)

parseStadium :: Node -> Stadium
parseStadium n = Stadium nm lts cap mdl pit
    where nm         = getStringAttr "name" n
          lts        = parsePointLightsXML (eChildren (fromMaybe nullNode lightsnode))
          cap        = getIntegerAttr "capacity" n
          mdl        = getStringAttr "file" $ fromMaybe nullNode modnode
          pit        = fromMaybe "" maypit
          maypit     = getMaybeStringAttr "pitch" n
          lightsnode = findNodeByName (eChildren n) "lights"
          modnode    = findNodeByName (eChildren n) "model"

-- n: <leaguesystem>
parseLeagueSystem :: Node -> LeagueSystem
parseLeagueSystem n = LeagueSystem entname entid lvls sched
    where entname       = fromMaybe "" (lookup "name" (eAttrs n))
          entid         = read (fromMaybe "-1" (lookup "id" (eAttrs n)))
          sched         = parseSchedule schednode
          lvls          = map (parseXMLNode parseBranch) (eChildren lvlsnode)
          schednode     = fromMaybe nullNode (findNodeByName (eChildren n) "schedule")
          lvlsnode      = fromMaybe nullNode (findNodeByName (eChildren n) "Levels")

-- n: <branch>
parseBranch :: Node -> Branch
parseBranch n = (stg, regentid)
    where stg       = parseStage stagenode
          regentid  = parseID (fromMaybe nullNode regnode)
          stagenode = fromMaybe nullNode (findNodeByName (eChildren n) "stage")
          regnode   = findNodeByName (eChildren n) "region"

parseID :: Node -> Integer
parseID n = read (fromMaybe "-1" (lookup "id" (eAttrs n)))

-- n: <stage>
parseStage :: Node -> Stage
parseStage n | fromMaybe "0" (lookup "type" (eAttrs n)) == "0" = League   nm   stageset   leagueset     proms rels  att troph
             | otherwise                                       = Knockout nm   stageset   knockoutset   ptarg rtarg troph
    where stageset      = StageSetup seed   parts rounds
          leagueset     = LeagueSetup ppw grps
          knockoutset   = KnockoutSetup et pen prl awg
          nm            = fromMaybe "" (lookup "name" (eAttrs n))
          seed          = (fromMaybe "0" seededval) /= "0"
          parts         = read (fromMaybe "1" partsval)
          grps          = read (fromMaybe "1" groupsval)
          rounds        = read (fromMaybe "1" roundsval)
          ppw           = read (fromMaybe "3" ppwval)
          et            = (fromMaybe "0" etval) /= "0"
          pen           = (fromMaybe "0" penval) /= "0"
          prl           = (fromMaybe "0" prlval) /= "0"
          awg           = (fromMaybe "0" awgval) /= "0"
          seededval     = lookup "seeded" (eAttrs (fromMaybe nullNode setupnode))
          partsval      = lookup "participantnum" (eAttrs (fromMaybe nullNode setupnode))
          groupsval     = lookup "groups" (eAttrs (fromMaybe nullNode setupnode))
          roundsval     = lookup "rounds" (eAttrs (fromMaybe nullNode setupnode))
          ppwval        = lookup "pointsperwin" (eAttrs (fromMaybe nullNode setupnode))
          etval         = lookup "extratime" (eAttrs (fromMaybe nullNode setupnode))
          penval        = lookup "penalties" (eAttrs (fromMaybe nullNode setupnode))
          prlval        = lookup "replays" (eAttrs (fromMaybe nullNode setupnode))
          awgval        = lookup "awaygoals" (eAttrs (fromMaybe nullNode setupnode))
          setupnode     = findNodeByName (eChildren n) "setup"
          proms         = map parsePromotion (findNodesByName (eChildren (fromMaybe nullNode promsnode)) "leaguepr")
          rels          = map parsePromotion (findNodesByName (eChildren (fromMaybe nullNode relsnode)) "leaguerl")
          promsnode     = findNodeByName (eChildren n) "leagueprs"
          relsnode      = findNodeByName (eChildren n) "leaguerls"
          ptarg         = parseStageTarget (fromMaybe nullNode ptargnode)
          ptargnode     = findNodeByName (eChildren n) "cuppr"
          rtarg         = parseStageTarget (fromMaybe nullNode rtargnode)
          rtargnode     = findNodeByName (eChildren n) "cuprl"
          att           = map parseAttendance (eChildren (fromMaybe nullNode attnode))
          attnode       = findNodeByName (eChildren n) "attendances"
          troph         = parseTrophy (fromMaybe nullNode trophynode)
          trophynode    = findNodeByName (eChildren n) "trophy"

parseAttendance :: Node -> (Int, Int, (String, String))
parseAttendance n = (hi, nu, (tr, st))
    where nu = fromMaybe 0 mn
          hi = fromMaybe 0 mh
          mn = getMaybeIntegerAttr "num" n
          mh = getMaybeIntegerAttr "highest" n
          tr = getStringAttr "tournament" n
          st = getStringAttr "stage" n

-- n: <leaguepr / leaguerl>
parsePromotion :: Node -> (Int, (String, String))
parsePromotion n = (num, st)
    where num    = read (fromMaybe "0" (lookup "num" (eAttrs n)))
          st     = parseStageTarget n

parseStageTarget :: Node -> (String, String)
parseStageTarget n = (ttarg, starg)
    where ttarg = fromMaybe "" mt
          mt    = lookup "tournament" (eAttrs n)
          starg = fromMaybe "" (lookup "stage" (eAttrs n))

-- n: <trophy>
parseTrophy :: Node -> Trophy
parseTrophy n = Trophy nm idnum_ img pts bn
    where nm         = fromMaybe "" (lookup "name" (eAttrs n))
          idnum_     = read (fromMaybe "-1" (lookup "id" (eAttrs n)))
          img        = fromMaybe "" (lookup "file" (eAttrs (fromMaybe nullNode imagenode)))
          pts        = read (fromMaybe "0" (lookup "points" (eAttrs (fromMaybe nullNode valuenode))))
          bn         = read (fromMaybe "0" (lookup "bonus"  (eAttrs (fromMaybe nullNode valuenode))))
          imagenode  = findNodeByName (eChildren n) "image"
          valuenode  = findNodeByName (eChildren n) "value"

-- n: <Tournaments>
parseTournamentXML' :: Node -> [Integer]
parseTournamentXML' n = [] -- TODO

-- n: <schedule>
parseSchedule :: Node -> Schedule
parseSchedule n = ((sm, sd), (em, ed))
    where sm = toEnum (read (fromMaybe "0" (lookup "sm" (eAttrs n))))
          sd = read (fromMaybe "0" (lookup "sd" (eAttrs n)))
          em = toEnum (read (fromMaybe "0" (lookup "em" (eAttrs n))))
          ed = read (fromMaybe "0" (lookup "ed" (eAttrs n)))

parsePlayerXML :: FilePath -> IO [Player]
parsePlayerXML file = parseXMLResource file "Players" parsePlayer

parsePlayer :: Node -> Player
parsePlayer n = Player idnum_ per personalit skls post contract
    where idnum_       = read $ fromMaybe "-1" $ lookup "id" $ eAttrs n
          per          = parsePersonal $ fromMaybe nullNode $ findNodeByName (eChildren n) "personal"
          personalit   = parsePersonality $ fromMaybe nullNode $ findNodeByName (eChildren n) "personality"
          skls         = parseSkills $ fromMaybe nullNode $ findNodeByName (eChildren n) "skills"
          post         = parsePosition $ fromMaybe nullNode $ findNodeByName (eChildren n) "position"
          contract     = if isNothing contractnode then "" else getStringAttr "name" $ fromMaybe nullNode contractnode
          contractnode = findNodeByName (eChildren n) "club"

parsePersonal :: Node -> Person
parsePersonal n = Person fn ln yob bd sc hc ec h nat
    where fn    = getStringAttr "first" n
          ln    = getStringAttr "last" n
          yob   = getIntegerAttr "year" bnode
          bd    = (bm, bday)
          bm    = toEnum $ getIntegerAttr "month" bnode - 1
          bday  = getIntegerAttr "day" bnode
          bnode = getChildOf n "birth"
          sc    = parseColor snode
          snode = getChildOf n "skin"
          hc    = parseColor hnode
          hnode = getChildOf n "hair"
          ec    = parseColor enode
          enode = getChildOf n "eyes"
          h     = getIntegerAttr "value" heign
          heign = getChildOf n "height"
          nat   = getStringAttr "value" natn
          natn  = getChildOf n "nationality"

parsePersonality :: Node -> PlayerPersonality
parsePersonality n = PlayerPersonality ac r o ag co cr ex
    where ac = getIntegerAttr "active" n
          r  = getIntegerAttr "risktaking" n
          o  = getIntegerAttr "offensive" n
          ag = getIntegerAttr "aggressive" n
          co = getIntegerAttr "consistent" n
          cr = getIntegerAttr "creative" n
          ex = getIntegerAttr "experienced" n

parseSkills :: Node -> PlayerSkills
parseSkills n = PlayerSkills st de sp ta pa sh co ac go he
    where st = getIntegerAttr "stamina" n
          de = getIntegerAttr "dexterity" n
          sp = getIntegerAttr "speed" n
          ta = getIntegerAttr "tackling" n
          pa = getIntegerAttr "passing" n
          sh = getIntegerAttr "shooting" n
          co = getIntegerAttr "control" n
          ac = getIntegerAttr "accuracy" n
          go = getIntegerAttr "goalkeeping" n
          he = getIntegerAttr "heading" n

parsePosition :: Node -> PlayerPosition
parsePosition n = newPlayerPosition p l w
    where p = getIntegerAttr "pos" n
          l = getBoolAttr "foot" n
          w = getBoolAttr "wing" n

parseClubXML :: FilePath -> IO [Club]
parseClubXML file = parseXMLResource file "Clubs" parseClub

parseClub :: Node -> Club
parseClub n = Club nm kits_ conts logo_ forms country_ region_ stadium_
    where nm           = getStringAttr "name" n
          kits_        = map parseKit (findNodesByName (eChildren (fromMaybe nullNode kitsnode)) "kit")
          kitsnode     = findNodeByName (eChildren n) "kits"
          conts        = map parseContract (findNodesByName (eChildren (fromMaybe nullNode contsnode)) "contract")
          contsnode    = findNodeByName (eChildren n) "contracts"
          logo_        = getStringAttr "name" (fromMaybe nullNode logonode)
          logonode     = findNodeByName (eChildren n) "logo"
          forms        = parseFormations $ fromMaybe nullNode formnode
          formnode     = findNodeByName (eChildren n) "formations"
          country_     = getStringAttr "name" $ fromMaybe nullNode $ findNodeByName (eChildren n) "country"
          region_      = getStringAttr "name" $ fromMaybe nullNode $ findNodeByName (eChildren n) "region"
          stadium_     = getStringAttr "name" $ fromMaybe nullNode $ findNodeByName (eChildren n) "stadium"

parseKit :: Node -> Kit
parseKit n = Kit jt jcs shc soc ji si
    where jt         = getIntegerAttr "type" jerseynode
          jerseynode = fromMaybe nullNode $ findNodeByName (eChildren n) "jersey"
          jcs        = map parseColor jcnodes
          jcnodes    = findNodesByName (eChildren jerseynode) "color"
          shc        = map parseColor shcnodes
          shortsnode = fromMaybe (Element "shorts" [] []) maybeshn
          maybeshn   = findNodeByName (eChildren n) "shorts"
          shcnodes   = findNodesByName (eChildren shortsnode) "color"
          soc        = map parseColor socnodes
          socksnode  = fromMaybe (Element "socks" [] []) maybeson
          maybeson   = findNodeByName (eChildren n) "socks"
          socnodes   = findNodesByName (eChildren socksnode) "color"
          ji         = getStringAttr "value" (fromMaybe nullNode jinode)
          jinode     = findNodeByName (eChildren jerseynode) "image"
          si         = getStringAttr "value" (fromMaybe nullNode sinode)
          sinode     = findNodeByName (eChildren shortsnode) "image"

parseContract :: Node -> Contract
parseContract n = Contract pl len sal bon
    where len  = fromMaybe 0 maybelen
          sal  = fromMaybe 0 maybesal
          bon  = fromMaybe 0 maybebon
          pl   = getBigIntegerAttr "player" n
          maybelen = getMaybeIntegerAttr "length" n
          maybesal = getMaybeIntegerAttr "salary" n
          maybebon = getMaybeIntegerAttr "bonus" n

parseFormations :: Node -> [String]
parseFormations n = map (getStringAttr "name") (findNodesByName (eChildren n) "formation")
