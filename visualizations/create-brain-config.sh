#Create brain visualization config file
declare -a ls=("banks_of_right_superior_temporal_sulcus" "caudalanteriorcingulate" "caudalmiddlefrontal" "cuneus" "entorhinal" "fusiform" "inferiorparietal" "inferiortemporal" "isthmuscingulate" "lateraloccipital" "lateralorbitofrontal" "lingual" "medialorbitofrontal" "middletemporal" "parahippocampal" "paracentral" "parsopercularis" "parsorbitalis" "parstriangularis" "pericalcarine" "postcentral" "posteriorcingulate" "precentral" "precuneus" "rostralanteriorcingulate" "rostralmiddlefrontal" "superiorfrontal" "superiorparietal" "superiortemporal" "supramarginal" "rh-frontal_pole" "temporal_pole" "transversetemporal" "insula")
i=1
len="${#ls[@]}"


#Lingual
if [ "$PARAMS1" = "Lingual" ]; then
	echo "[
    {
        \"name\": \"ctx-lh-lingual\",
        \"pval\": $p_val
    },
    {
        \"name\": \"ctx-rh-lingual\",
        \"pval\": $p_val
    }
	]" > $OUTPUTS1
    
    
#Precentral
elif [ "$PARAMS1" = "Precentral" ]; then 
	echo "[
    {
        \"name\": \"ctx-lh-precentral\",
        \"pval\": $p_val
    },
    {
        \"name\": \"ctx-rh-precentral\",
        \"pval\": $p_val
    }
	]" > $OUTPUTS1
    
    
#Precuneus
elif [ "$PARAMS1" = "Precuneus" ]; then 
	echo "[
    {
        \"name\": \"ctx-lh-precuneus\",
        \"pval\": $p_val
    },
    {
        \"name\": \"ctx-rh-precuneus\",
        \"pval\": $p_val
    }
	]" > $OUTPUTS1
    
    
#Pericalcarine
elif [ "$PARAMS1" = "Pericalcarine" ]; then 
	echo "[
    {
        \"name\": \"ctx-lh-pericalcarine\",
        \"pval\": $p_val
    },
    {
        \"name\": \"ctx-rh-pericalcarine\",
        \"pval\": $p_val
    }
	]" > $OUTPUTS1
    
    
#RostralMiddleFrontal   
elif [ "$PARAMS1" = "RostralMiddleFrontal" ]; then 
	echo "[
    {
        \"name\": \"ctx-lh-rostralmiddlefrontal\",
        \"pval\": $p_val
    },
    {
        \"name\": \"ctx-rh-rostralmiddlefrontal\",
        \"pval\": $p_val
    }
	]" > $OUTPUTS1
    
    
#InferiorParietal
elif [ "$PARAMS1" = "InferiorParietal" ]; then 
	echo "[
    {
        \"name\": \"ctx-lh-inferiorparietal\",
        \"pval\": $p_val
    },
    {
        \"name\": \"ctx-rh-inferiorparietal\",
        \"pval\": $p_val
    }
	]" > $OUTPUTS1
    
#TotalSA and AvgTH
elif [ "$PARAMS1" = "TotalSA" ] || [ "$PARAMS1" = "AvgTH" ]; then
	for var in "${ls[@]}"
	do
	if [ "$i" -eq "1" ]; then
		echo "
		[
		    {
		        \"name\": \"ctx-lh-$var\",
		        \"pval\": $p_val
		    },

		    {
		        \"name\": \"ctx-rh-$var\",
		        \"pval\": $p_val
		    }, "
		i=$((i+1))

	elif [ "$i" -eq "$len" ]; then
		echo "
			{
		        \"name\": \"ctx-lh-$var\",
		        \"pval\": $p_val
		    },

		    {
		        \"name\": \"ctx-rh-$var\",
		        \"pval\": $p_val
		    }
		]"
		i=$((i+1))

	else
		echo "
			{
		        \"name\": \"ctx-lh-$var\",
		        \"pval\": $p_val
		    },

		    {
		        \"name\": \"ctx-rh-$var\",
		        \"pval\": $p_val
		    }, "
		i=$((i+1))
	fi        
	done > $OUTPUTS1

else 
	echo "[]" > $OUTPUTS1
fi
