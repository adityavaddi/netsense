MATCH (p:DHProfile {dhprofileid: {props}.dhprofileid})
SET p.setPoint = {props}.setPoint
    REMOVE p.autocalibrate
