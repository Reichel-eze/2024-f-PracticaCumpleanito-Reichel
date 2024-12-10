module Library where
import PdePreludat

data Invitade = UnInvitade {
    cansancio :: Number,
    felicidad :: Number,
    cancionFavorita :: String
}deriving(Show, Eq)

ceci :: Invitade
ceci = UnInvitade {
    cansancio = 79,
    felicidad = 85,
    cancionFavorita = "Tango lloron"
}

valentin :: Invitade
valentin = UnInvitade {
    cansancio = 99,
    felicidad = 150,
    cancionFavorita = "No se"
}

eze :: Invitade
eze = UnInvitade {
    cansancio = 70,
    felicidad = 100,
    cancionFavorita = "Labios Compartidos"
}

-- 1. La previa

estaCansade :: Invitade -> Bool
estaCansade invitado = cansancio invitado > 80

cansarse :: Invitade -> Invitade
cansarse invitado = invitado {cansancio = cansancio invitado + 10}

disfrutar :: Invitade -> Invitade
disfrutar invitado = invitado {felicidad = felicidad invitado + 100 - cansancio invitado}

-- 2. El cachengue
-- Durante la fiesta, les invitades van haciendo distintos plancitos que afectan sus estados. 

type Plancito = Invitade -> Invitade

charlitaDeFulbo :: Plancito
charlitaDeFulbo = disfrutar 

-- 1ero disfruta, 2do se cansa
bailar :: Plancito
bailar = cansarse . disfrutar

-- 1ero se cansa, 2do disfruta
mesaDulce :: Plancito
mesaDulce = disfrutar . cansarse

-- Cuando se cancion favorita tiene una cantidad par de caracteres
tieneBuenGusto :: Invitade -> Bool
tieneBuenGusto = even . length . cancionFavorita 

tieneBuenGusto' :: Invitade -> Bool
tieneBuenGusto' invitado = even (length (cancionFavorita invitado)) 

-- Consultar si despues de hacer un plancito, la persona va a estar cansada
-- 1ero. Mando al invitado a realizar el plancito
-- 2dos. Luego analizo si esta cansado luego del plan 
leVaADarFiaca :: Plancito -> Invitade -> Bool
leVaADarFiaca plancito invitado = estaCansade (plancito invitado) 

-- 3. Y que cumplas muchos mas

type Bandurria = [Invitade]

-- A
-- 1ero. Busco la favorita de un invitade           (funcion transformadora)
-- 2dos. Mapeo, se lo aplico a todos los elementos  (parametro a transformar)
playlist :: Bandurria -> [String]
playlist bandurria = map cancionFavorita bandurria

-- B
lesQueLaSigen :: Bandurria -> Bandurria
lesQueLaSigen bandurria = filter (not . estaCansade) bandurria

-- C
todosHaganUnPlan :: Plancito -> Bandurria -> Bandurria
todosHaganUnPlan plancito bandurria = map plancito bandurria

-- D
laRompe :: Bandurria -> Bool
laRompe bandurria = all (not . estaCansade) bandurria

-- E
vaASonarUnHitazo :: Bandurria -> Bool
vaASonarUnHitazo bandurria = any tieneBuenGusto bandurria

-- F
vaAArmarUnFieston :: Bandurria -> Bool
vaAArmarUnFieston bandurria = (sum . map felicidad . lesQueLaSigen) bandurria > 300

-- G. Esto significa que después de que todes hagan el plancito se va a armar un fiestón
seleSube :: Plancito -> Bandurria -> Bool
seleSube plancito bandurria = (vaAArmarUnFieston . todosHaganUnPlan plancito) bandurria