#' Argument Flag check
#'
#' check to make sure arguments passed bby user do not contradict each other.
#'
#' @inheritParams get_comland_data
#'
#' @noRd

# We can either create error messages (like below) or fix argument specs

check_argument_validation <- function(aggArea,
                                      userAreas,
                                      areaDescription,
                                      propDescription,
                                      applyProp,
                                      aggGear,
                                      userGears,
                                      fleetDescription,
                                      unkVar,
                                      knStrata) {

  ############## AggArea ###############

  # if aggArea = T then userAreas, areaDescription and propDescription
  # follow rules
  if(aggArea) {
    # field names of userAreas
    if(!(areaDescription %in% names(userAreas))) {
      stop(paste0(areaDescription, " is not a field name in userAreas object"))
    }
    if(!(propDescription %in% names(userAreas))) {
      stop(paste0(propDescription, " is not a field name in userAreas object"))
    }
    if(!("AREA" %in% names(userAreas))) {
      stop(paste0("AREA is not a field name in userAreas object.
      This object is used to aggregate Statistical Areas to larger regional units.
      Field names must include: AREA and 'areaDescription' "))
    }
  }

  ############## AggGear ###############

  # if aggGear = T then userGears, fleetDescription follow rules
  if(aggGear) {
    # field names of userGears
    if(!(fleetDescription %in% names(userGears))) {
      stop(paste0(fleetDescription, " is not a field name in userGears object"))
    }
  }

  ############## applyProp ###############

  # applyProp currently relies on a specific format of userAreas
  if(applyProp) {
    if (!aggArea) {
      stop("Can not have 'aggArea = F' if you want to propotion landings ('applyProp = T') by
           statistical area to a larger spatial unit")
    }

    # if aggArea = T hen these conditions should be met
    if(!("NESPP3" %in% names(userAreas))) {
      stop(paste0("NESPP3 is not a field name in userAreas object.
      This object is used to aggregate Statistical Areas to larger regional units and
      proportion landings to these larger regional unit depening on the species and Statistical area.
      Field names must include: NESPP3, AREA and 'areaDescription' "))
    }

    stop("Proportion allocation is currently not implemented correctly.
         Please use 'applyProp = F'")

  }

  ################ UNKNOWNS #################

  # checks for filling in missing values (assign_unknowns)
  # Depending on the flags above will determine how the arguments unkVar and knStrata are defined
  # if (aggArea) {
  #   if (!(areaDescription %in% unkVar) | !(areaDescription %in% knStrata)) {
  #     stop(paste0("To assign unknowns when using 'aggArea = T', then you need to replace
  #          'AREA' with '" ,areaDescription,"' in both 'unkVar' and 'knStrata' arguments"))
  #   }
  # } else {
  #   if (!("AREA" %in% unkVar) | !("AREA" %in% knStrata)) {
  #     stop(paste0("To assign unknowns when using 'aggArea = F', then you need to use
  #          'AREA' in both 'unkVar' and 'knStrata' arguments"))
  #   }
  #
  # }

  # if (aggGear) {
  #   if (!(fleetDescription %in% unkVar) | !(fleetDescription %in% knStrata)) {
  #     stop(paste0("To assign unknowns when using 'aggGear = T', then you need to replace
  #               'NEGEAR' with '" ,fleetDescription,"' in both 'unkVar' and 'knStrata' arguments"))
  #   }
  # } else {
  #   if (!("NEGEAR" %in% unkVar) | !("NEGEAR" %in% knStrata)) {
  #     stop(paste0("To assign unknowns when using 'aggGear = F', then you need to use
  #          'NEGEAR' in both 'unkVar' and 'knStrata' arguments"))
  #   }
  # }




  # check for character or numeric

}
