##define data classes

setClass("SystemData",
         representation(entityList = "data.frame",
                        dpList = "data.frame",
                        entityDPMatrix = "matrix",
                        constList = "data.frame",
                        constDPMatrix ="matrix",
                        dpdpMatrix = "matrix"));


setClass("PropagationPath",
         representation(id = "character",
                        requirementConflict = "numeric",
                        requiredResource = "numeric",
                        solutionArea = "numeric",
                        path = "list",
                        dps = "character",
                        constraints = "character",
                        constDPMatrix ="matrix"))