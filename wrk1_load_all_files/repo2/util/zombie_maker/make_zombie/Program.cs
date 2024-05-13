using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Diagnostics;



namespace make_zombie
{
    internal class Program{
        static void Main(string[] args){

            ProcessStartInfo si = new ProcessStartInfo();
            si.WindowStyle = ProcessWindowStyle.Hidden;
            si.FileName = "excel.exe";
            si.Arguments=""
            write_exception_log_entry("DataPath: " + Application.UserAppDataPath);

            try { 
                for(int argindex=0; argindex<args.Length; argindex++) {
                    write_exception_log_entry(" Arg " + argindex + ": " + args[argindex]);
                }
                
                if (args.Length == 1)
                {
                    System.Diagnostics.Process.Start(args[0]);
                }
                else{
                    System.Diagnostics.Process.Start(args[0], args[1]);
                }
            }
            catch(Exception ex) {
                write_exception_to_log(ex);
            }
                            

        }
        private static void write_exception_log_entry(string line) {
            string path = Application.UserAppDataPath;
            string Filename = path + "\\makezombie_error.log";
            StreamWriter s = File.AppendText(Filename);
            try { 
                s.WriteLine(line);
            }
            finally { 
                s.Close();
            }

        }
        private static void write_exception_to_log(Exception ex) {

            write_exception_log_entry("" + ex);

        }
    }
}
