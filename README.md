# Food demand forecast
To do list
* analisi esplorativa univariata per MEAL, SALES
* rimuovere o no checkout price?
* analisi bivariata tra num_orders e le altre variabili concomitanti

Approccio 1:
* modello lineare per num_orders condizionato a meal_id e id_center. Si dovrebbe poter fare tutti i modelli in un colpo solo gestendo bene le dummy. 
* analisi residui? giusto perché la prof ha il fetish a riguardo
* gradient boosting EXTREME sui residui, dovrebbe essere una buona idea si spera


Approccio 2:
* time window e XGBoost EXTREME boomboom e si vede la performance

Problemi:
* Come selezioniamo il modello?
* Cross validation con dati sequenziali è un po' problematica
