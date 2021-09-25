using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;

namespace PrompterMaxF
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        public App()
        {
            Activated += App_Activated;
        }

        private void App_Activated(object sender, EventArgs e)
        {
            Activated -= App_Activated;
            Prompter.AppMain.main(MainWindow, () => new ExtractorWindow());
        }
    }
}
