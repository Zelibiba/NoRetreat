using Avalonia;
using Avalonia.Data.Converters;
using Avalonia.Media;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HexGameControls.Converters
{
    internal class HexItemConverter : IMultiValueConverter
    {
        public object? Convert(IList<object?> values, Type targetType, object? parameter, CultureInfo culture)
        {
            if (values[0] is not double width) return null;
            if (values[1] is not double height) return null;
            if (width <= 0 || height <= 0) return null;

            double thick = (double?)values.ElementAtOrDefault(2) ?? 0;
            width -= thick;
            height -= thick;
            thick /= 2;

            return new PathGeometry
            {
                Figures =
                {
                    new PathFigure
                    {
                        StartPoint = new Point(thick + width*0.5, thick),
                        IsClosed = true,
                        Segments =
                        {
                            new LineSegment { Point = new Point(thick + width, thick + height*0.25) },
                            new LineSegment { Point = new Point(thick + width, thick + height*0.75) },
                            new LineSegment { Point = new Point(thick + width*0.5, thick + height) },
                            new LineSegment { Point = new Point(thick, thick + height*0.75) },
                            new LineSegment { Point = new Point(thick, thick + height*0.25) },
                        }
                    }
                }
            };
        }

        public object[] ConvertBack(IList<object?> values, Type targetType, object? parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}
