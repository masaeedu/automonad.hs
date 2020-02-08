module CT.Functor.Product where

-- A category can be closed, in which case it must have some well-behaved "internal hom" bifunctor
--
-- A category can be monoidal, in which case it has a well-behaved tensor and unit
--
-- A category can be cartesian, in which case it is monoidal under the categorical product and terminal object
--
-- A category can be closed-monoidal, in which case the monoidal structure and closed structure interact
--
-- A category can be cartesian closed, in which case it is closed monoidal and the tensor is the cartesian product
--
-- Why am I talking about all this stuff?
--
-- Well, when a category is cartesian closed, the functor category to it is also cartesian closed.
