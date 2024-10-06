import os
import subprocess

def convert_single_ps_to_pdf(ps_file):
    if not ps_file.endswith(".ps"):
        print("The file is not a .ps file.")
        return
    
    pdf_file = os.path.splitext(ps_file)[0] + ".pdf"
    
    try:
        subprocess.run(["ps2pdf", ps_file, pdf_file], check=True)
        print(f"Successfully converted {ps_file} to {pdf_file}")
    except subprocess.CalledProcessError as e:
        print(f"Failed to convert {ps_file}: {e}")

ps_file_path = './waveform_plot.ps'
convert_single_ps_to_pdf(ps_file_path)
