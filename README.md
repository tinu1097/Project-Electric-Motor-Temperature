# Project-Electric_Motor_Temperature
# Deployement on Shinyapps.io#
# App Link#
# https://teena-agrawal.shinyapps.io/shinyDep/


# Business Objective
Predict temperature based on other attributes available in this data set output variable Motor Temperature(pm) and rest of the coloumns ambient,coolant,u_d,u_q,motor_speed,torque,i_d,i_q,pm,stator_yoke,stator_tooth,stator_einding,profile_id.

## Details of the Feature set:
Ambient temperature as measured by a thermal sensor located closely to the stator,Coolant temperature. The motor is water cooled. Measurement is taken at outflow.u_d-Voltage d-component,u_q-Voltage q-component motor_speed,Motor speed,Torque induced by current,i_d-Current d-component,i_q-Current q-component,pm-Permanent Magnet surface temperature representing the rotor temperature. This was measured with an infrared thermography unit,Stator yoke temperature measured with a thermal sensor,Stator tooth temperature measured with a thermal sensor,Stator winding temperature measured with a thermal sensor,profile_id-Each measurement session has a unique ID. Make sure not to try to estimate from one session onto the other as they are strongly independent.

# Explaination
Measuring the temperature of motor is complex task in real time scenario. The cost of sensor is high and also lifetime of those are less.But also testing every motor’s temperature by placing sensors inside the Stator is not possible.  
By using Machine learning algorithms , the task is made easier. The accuracy by my model is 94.35% and also, I deployed the model using Rshiny  where prediction without dataset is also possible by imputing values in their respective fields 
