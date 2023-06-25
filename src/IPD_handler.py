import csv
import os

IPD_location = "/home/matthew/Documents/PCNMA/Data/IPD/"

os.chdir(IPD_location)

for i in os.listdir():
    study = i.split("_")[2].strip(".csv")
    paramcd = "OS" if "OS" in i.split("_")[1] else "PFS"
    treatment = "GEM" if "GemO" or "GemP" in i else i.split("_")[1].strip(paramcd)

    with open(i, "r") as file:
        reader = csv.reader(file)
        header = next(reader)

        if "Study" in header:
            print("study contained in data already")
        
        else:
            header.append("Study")

            temp_path = "temp_"+i
            with open(temp_path, "w", newline = "") as temp_file:
                writer = csv.writer(temp_file)
                writer.writerow(header)


                for row in reader:
                    row.append(study)
                    writer.writerow(row)
        
            os.system("rm "+i+" & mv temp_"+i+" "+i)

    with open(i, "r") as file:
        reader = csv.reader(file)
        header = next(reader)

        if "PARAMCD" in header:
            print("PARAMCD contained in data already")

        else:
            header.append("PARAMCD")

            temp_path = "temp_"+i
            with open(temp_path, "w", newline = "") as temp_file:
                writer = csv.writer(temp_file)
                writer.writerow(header)


                for row in reader:
                    row.append(paramcd)
                    writer.writerow(row)
        
            os.system("rm "+i+" & mv temp_"+i+" "+i)

    with open(i, "r") as file:
        reader = csv.reader(file)
        header = next(reader)

        if "Treatment" in header:
            print("Treatment contained in data already")

        else:
            header.append("Treatment")

            temp_path = "temp_"+i
            with open(temp_path, "w", newline = "") as temp_file:
                writer = csv.writer(temp_file)
                writer.writerow(header)


                for row in reader:
                    row.append(treatment)
                    writer.writerow(row)
        
            os.system("rm "+i+" & mv temp_"+i+" "+i)          